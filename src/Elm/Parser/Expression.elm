module Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Parser.Whitespace as Whitespace
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing ((|=), Nestable(..))
import Pratt exposing (Config)


expression : Parser State (Node Expression)
expression =
    Pratt.expression
        -- TODO make sure that operators and expressions are indented properly
        { oneOf =
            [ referenceExpression
                |> Pratt.literal
            , literalExpression
                |> Pratt.literal
            , numberExpression
                |> Pratt.literal
            , tupledExpression
            , glslExpression
                |> Pratt.literal
            , listExpression
            , recordExpression
            , caseExpression
            , lambdaExpression
            , letExpression
            , ifBlockExpression
            , recordAccessFunctionExpression
                |> Pratt.literal
            , negationOperation
            , charLiteralExpression
                |> Pratt.literal
            ]
        , andThenOneOf =
            -- TODO Add tests for all operators
            -- TODO Report a syntax error when encountering multiple of the comparison operators
            -- `a < b < c` is not valid Elm syntax
            [ recordAccess
            , infixLeft 1 "|>"
            , infixRight 5 "++"
            , infixRight 1 "<|"
            , infixRight 9 ">>"
            , infixNonAssociative 4 "=="
            , infixLeft 7 "*"
            , infixRight 5 "::"
            , infixLeft 6 "+"
            , infixLeftSubtraction 6
            , infixLeft 6 "|."
            , infixRight 3 "&&"
            , infixLeft 5 "|="
            , infixLeft 9 "<<"
            , infixNonAssociative 4 "/="
            , infixLeft 7 "//"
            , infixLeft 7 "/"
            , infixRight 7 "</>"
            , infixRight 2 "||"
            , infixNonAssociative 4 "<="
            , infixNonAssociative 4 ">="
            , infixNonAssociative 4 ">"
            , infixLeft 8 "<?>"
            , infixNonAssociative 4 "<"
            , infixRight 8 "^"

            -- function application must be last
            -- TODO validate function application arguments (issue #209)
            , functionCall
            ]
        , spaces = Layout.optimisticLayout |> Combine.map (always ())
        }


infixLeft : Int -> String -> Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
infixLeft precedence symbol =
    Pratt.infixLeft precedence
        (Combine.symbol symbol)
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
infixNonAssociative precedence symbol =
    Pratt.infixLeft precedence
        (Combine.symbol symbol)
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
infixRight precedence symbol =
    Pratt.infixRight precedence
        (Combine.symbol symbol)
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication symbol Infix.Right left right)
        )


infixLeftSubtraction : Int -> Config State (Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftSubtraction precedence =
    Pratt.infixLeft precedence
        (Core.succeed (\offset source -> String.slice (offset - 1) offset source)
            |= Core.getOffset
            |= Core.getSource
            |> Combine.fromCore
            |> Combine.andThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        Combine.oneOf
                            [ Combine.symbol "- "
                            , Combine.symbol "-\n"
                            , Combine.symbol "-\u{000D}"
                            ]

                    else
                        Combine.symbol "-"
                )
        )
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication "-" Infix.Left left right)
        )


recordAccess : Config State (Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
recordAccess =
    Pratt.postfix 100
        recordAccessParser
        (\((Node leftRange _) as left) ((Node rightRange _) as field) ->
            Node
                { start = leftRange.start, end = rightRange.end }
                (Expression.RecordAccess left field)
        )


recordAccessParser : Parser State (Node String)
recordAccessParser =
    Core.succeed (\offset source -> String.slice (offset - 1) offset source)
        |= Core.getOffset
        |= Core.getSource
        |> Combine.fromCore
        |> Combine.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    Combine.fail "Record access can't start with a space"

                else
                    Combine.string "."
                        |> Combine.continueWith (Node.parser Tokens.functionName)
            )


functionCall : Pratt.Config State (Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
functionCall =
    Pratt.infixLeft 90
        Layout.positivelyIndented
        (\((Node leftRange leftValue) as left) right ->
            case leftValue of
                Expression.Application args ->
                    Node
                        { start = leftRange.start, end = (Node.range right).end }
                        (Expression.Application (args ++ [ right ]))

                _ ->
                    Node
                        { start = leftRange.start, end = (Node.range right).end }
                        (Expression.Application [ left, right ])
        )


glslExpression : Parser State (Node Expression)
glslExpression =
    let
        start : String
        start =
            "[glsl|"

        end : String
        end =
            "|]"
    in
    Core.getChompedString (Core.multiComment start end NotNestable)
        |> Combine.fromCore
        |> Combine.map (String.dropLeft (String.length start) >> GLSLExpression)
        |> Combine.ignore (Combine.string end)
        |> Node.parser


listExpression : Config State (Node Expression) -> Parser State (Node Expression)
listExpression config =
    Combine.succeed ListExpr
        |> Combine.ignore (Combine.string "[")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep
            (Combine.sepBy
                (Combine.string ",")
                (Pratt.subExpression 0 config)
            )
        |> Combine.ignore (Combine.string "]")
        |> Node.parser



-- recordExpression


recordExpression : Config State (Node Expression) -> Parser State (Node Expression)
recordExpression config =
    Combine.string "{"
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.string "}" |> Combine.map (always (RecordExpr []))
                , recordContents config
                ]
            )
        |> Node.parser


recordContents : Config State (Node Expression) -> Parser State Expression
recordContents config =
    Node.parser Tokens.functionName
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser config fname
                    , Combine.string "="
                        |> Combine.continueWith (Pratt.subExpression 0 config)
                        |> Combine.andThen
                            (\e ->
                                let
                                    fieldUpdate : Node RecordSetter
                                    fieldUpdate =
                                        Node.combine Tuple.pair fname e
                                in
                                Combine.oneOf
                                    [ Combine.string "}"
                                        |> Combine.map (always (RecordExpr [ fieldUpdate ]))
                                    , Combine.succeed (\fieldUpdates -> RecordExpr (fieldUpdate :: fieldUpdates))
                                        |> Combine.ignore (Combine.string ",")
                                        |> Combine.ignore (Combine.maybe Layout.layout)
                                        |> Combine.keep (recordFields config)
                                        |> Combine.ignore (Combine.string "}")
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Config State (Node Expression) -> Node String -> Parser State Expression
recordUpdateSyntaxParser config fname =
    Combine.succeed (\e -> RecordUpdateExpression fname e)
        |> Combine.ignore (Combine.string "|")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep (recordFields config)
        |> Combine.ignore (Combine.string "}")


recordFields : Config State (Node Expression) -> Parser State (List (Node RecordSetter))
recordFields config =
    Combine.succeed (::)
        |> Combine.keep (recordField config)
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep
            (Combine.many
                (Combine.string ","
                    |> Combine.ignore (Combine.maybe Layout.layout)
                    |> Combine.continueWith (recordField config)
                    |> Combine.ignore (Combine.maybe Layout.layout)
                )
            )


recordField : Config State (Node Expression) -> Parser State (Node RecordSetter)
recordField config =
    Node.parser
        (Combine.succeed Tuple.pair
            |> Combine.keep (Node.parser Tokens.functionName)
            |> Combine.ignore (Combine.maybe Layout.layout)
            |> Combine.ignore (Combine.string "=")
            |> Combine.keep (Pratt.subExpression 0 config)
        )


literalExpression : Parser State (Node Expression)
literalExpression =
    Combine.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Combine.map Literal
        |> Node.parser


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Node.parser (Combine.map CharLiteral Tokens.characterLiteral)



-- lambda


lambdaExpression : Config State (Node Expression) -> Parser State (Node Expression)
lambdaExpression config =
    Combine.succeed
        (\(Node { start } _) args expr ->
            Lambda args expr
                |> LambdaExpression
                |> Node { start = start, end = (Node.range expr).end }
        )
        |> Combine.keep (Node.parser (Combine.string "\\"))
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep (Combine.sepBy1 (Combine.maybe Layout.layout) Patterns.pattern)
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore (Combine.string "->")
        |> Combine.keep (Pratt.subExpression 0 config)



-- Case Expression


caseExpression : Config State (Node Expression) -> Parser State (Node Expression)
caseExpression config =
    Combine.succeed
        (\caseKeyword caseBlock_ ( end, cases ) ->
            Node { start = (Node.range caseKeyword).start, end = end }
                (CaseExpression (CaseBlock caseBlock_ cases))
        )
        |> Combine.keep (Node.parser Tokens.caseToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Pratt.subExpression 0 config)
        |> Combine.ignore Layout.positivelyIndented
        |> Combine.ignore Tokens.ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState (caseStatements config))


caseStatements : Config State (Node Expression) -> Parser State ( Location, Cases )
caseStatements config =
    Combine.many1WithEndLocationForLastElement (\( _, Node range _ ) -> range) (caseStatement config)


caseStatement : Config State (Node Expression) -> Parser State Case
caseStatement config =
    Combine.succeed Tuple.pair
        |> Combine.ignore Layout.onTopIndentation
        |> Combine.keep Patterns.pattern
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore (Combine.string "->")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep (Pratt.subExpression 0 config)



-- Let Expression


letExpression : Config State (Node Expression) -> Parser State (Node Expression)
letExpression config =
    Combine.succeed (\( Node { start } _, decls ) expr -> Node { start = start, end = (Node.range expr).end } (LetBlock decls expr |> LetExpression))
        |> Combine.keep
            (withIndentedState
                (Combine.succeed Tuple.pair
                    |> Combine.keep (Node.parser (Combine.string "let"))
                    |> Combine.ignore Layout.layout
                    |> Combine.keep (withIndentedState (letBody config))
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.ignore (Combine.string "in")
                )
            )
        |> Combine.keep (Pratt.subExpression 0 config)


letBody : Config State (Node Expression) -> Parser State (List (Node LetDeclaration))
letBody config =
    Combine.many1 (blockElement config)


blockElement : Config State (Node Expression) -> Parser State (Node LetDeclaration)
blockElement config =
    Layout.onTopIndentation
        |> Combine.continueWith Patterns.pattern
        |> Combine.andThen
            (\(Node r p) ->
                case p of
                    Pattern.VarPattern v ->
                        functionWithNameNode config (Node r v)
                            |> Combine.map (\fn -> Node (Expression.functionRange fn) (LetFunction fn))

                    _ ->
                        letDestructuringDeclarationWithPattern config (Node r p)
            )


letDestructuringDeclarationWithPattern : Config State (Node Expression) -> Node Pattern -> Parser State (Node LetDeclaration)
letDestructuringDeclarationWithPattern config pattern =
    Combine.succeed
        (\expr ->
            Node { start = (Node.range pattern).start, end = (Node.range expr).end } (LetDestructuring pattern expr)
        )
        |> Combine.ignore (Combine.string "=")
        |> Combine.keep (Pratt.subExpression 0 config)


numberExpression : Parser State (Node Expression)
numberExpression =
    Node.parser (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)


ifBlockExpression : Config State (Node Expression) -> Parser State (Node Expression)
ifBlockExpression config =
    Combine.succeed
        (\(Node { start } _) condition ifTrue ifFalse ->
            Node
                { start = start, end = (Node.range ifFalse).end }
                (IfBlock condition ifTrue ifFalse)
        )
        |> Combine.keep (Node.parser Tokens.ifToken)
        |> Combine.keep (Pratt.subExpression 0 config)
        |> Combine.ignore Tokens.thenToken
        |> Combine.keep (Pratt.subExpression 0 config)
        |> Combine.ignore Tokens.elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (Pratt.subExpression 0 config)


negationOperation : Config s (Node Expression) -> Parser s (Node Expression)
negationOperation =
    Pratt.prefix 95
        minusNotFollowedBySpace
        (\((Node { start, end } _) as subExpr) ->
            Node
                { start = { row = start.row, column = start.column - 1 }, end = end }
                (Negation subExpr)
        )


minusNotFollowedBySpace : Parser s ()
minusNotFollowedBySpace =
    Combine.succeed identity
        |> Combine.ignore (Combine.backtrackable (Combine.string "-"))
        |> Combine.keep
            (Combine.oneOf
                [ Combine.map (always True) (Combine.backtrackable Whitespace.realNewLine)
                , Combine.map (always True) (Combine.backtrackable (Combine.string " "))
                , Combine.succeed False
                ]
            )
        |> Combine.andThen
            (\isSpaceOrComment ->
                if isSpaceOrComment then
                    Combine.fail "negation sign cannot be followed by a space"

                else
                    Combine.fromCore (Core.commit ())
            )


referenceExpression : Parser State (Node Expression)
referenceExpression =
    let
        helper : ModuleName -> String -> Parser s Expression
        helper moduleNameSoFar nameOrSegment =
            Combine.oneOf
                [ Combine.string "."
                    |> Combine.continueWith
                        (Combine.oneOf
                            [ Tokens.typeName
                                |> Combine.andThen (\t -> helper (nameOrSegment :: moduleNameSoFar) t)
                            , Tokens.functionName
                                |> Combine.map
                                    (\name ->
                                        FunctionOrValue
                                            (List.reverse (nameOrSegment :: moduleNameSoFar))
                                            name
                                    )
                            ]
                        )
                , Combine.succeed ()
                    |> Combine.map (\() -> FunctionOrValue (List.reverse moduleNameSoFar) nameOrSegment)
                ]
    in
    Combine.oneOf
        [ Tokens.typeName
            |> Combine.andThen (\t -> helper [] t)
        , Tokens.functionName
            |> Combine.map (\v -> FunctionOrValue [] v)
        ]
        |> Node.parser


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Combine.succeed (\field -> RecordAccessFunction ("." ++ field))
        |> Combine.ignore (Combine.string ".")
        |> Combine.keep Tokens.functionName
        |> Node.parser


tupledExpression : Config State (Node Expression) -> Parser State (Node Expression)
tupledExpression config =
    let
        asExpression : Node Expression -> List (Node Expression) -> Expression
        asExpression x xs =
            case xs of
                [] ->
                    ParenthesizedExpression x

                _ ->
                    TupledExpression (x :: xs)

        commaSep : Parser State (List (Node Expression))
        commaSep =
            Combine.many
                (Combine.string ","
                    |> Combine.continueWith (Pratt.subExpression 0 config)
                )

        nested : Parser State Expression
        nested =
            Combine.succeed asExpression
                |> Combine.keep (Pratt.subExpression 0 config)
                |> Combine.keep commaSep

        closingParen : Parser state ()
        closingParen =
            Combine.symbol ")"
    in
    Combine.symbol "("
        |> Combine.continueWith
            (Combine.oneOf
                [ closingParen |> Combine.map (always UnitExpr)
                , Combine.backtrackable Tokens.prefixOperatorToken
                    |> Combine.ignore closingParen
                    |> Combine.ignore (Combine.fromCore (Core.commit ()))
                    |> Combine.map PrefixOperator
                , nested |> Combine.ignore closingParen
                ]
            )
        |> Node.parser


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    Combine.withLocation
        (\location ->
            Combine.modifyState (State.pushIndent location.column)
                |> Combine.continueWith p
                |> Combine.ignore (Combine.modifyState State.popIndent)
        )


functionWithNameNode : Config State (Node Expression) -> Node String -> Parser State Function
functionWithNameNode config pointer =
    let
        functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
        functionImplementationFromVarPointer varPointer =
            Combine.succeed (\args expr -> Node { start = (Node.range varPointer).start, end = (Node.range expr).end } (FunctionImplementation varPointer args expr))
                |> Combine.keep (Combine.many (Patterns.pattern |> Combine.ignore (Combine.maybe Layout.layout)))
                |> Combine.ignore (Combine.string "=")
                |> Combine.keep (Pratt.subExpression 0 config)

        fromParts : Node Signature -> Node FunctionImplementation -> Function
        fromParts sig decl =
            { documentation = Nothing
            , signature = Just sig
            , declaration = decl
            }

        functionWithSignature : Node String -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.ignore (Combine.maybe Layout.layoutStrict)
                |> Combine.andThen
                    (\sig ->
                        Node.parser Tokens.functionName
                            |> Combine.andThen (failIfDifferentFrom varPointer)
                            |> Combine.ignore (Combine.maybe Layout.layout)
                            |> Combine.andThen functionImplementationFromVarPointer
                            |> Combine.map (fromParts sig)
                    )

        functionWithoutSignature : Node String -> Parser State Function
        functionWithoutSignature varPointer =
            functionImplementationFromVarPointer varPointer
                |> Combine.map (Function Nothing Nothing)
    in
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


failIfDifferentFrom : Node String -> Node String -> Parser State (Node String)
failIfDifferentFrom (Node _ expectedName) ((Node _ actualName) as actual) =
    if expectedName == actualName then
        Combine.succeed actual

    else
        Combine.fail <| "Expected to find the declaration for " ++ expectedName ++ " but found " ++ actualName


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    Combine.succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignore (Combine.string ":")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep TypeAnnotation.typeAnnotation
