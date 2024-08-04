module Elm.Parser.Expression exposing (expression)

import CustomParser exposing (Parser)
import CustomParser.Advanced
import CustomParser.Extra
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    CustomParser.map
        (\( startRow, startColumn ) ->
            \expressionAndEnd ->
                { comments = expressionAndEnd.comments
                , syntax =
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = expressionAndEnd.end
                        }
                        expressionAndEnd.expression
                }
        )
        CustomParser.getPosition
        |> CustomParser.keep
            (CustomParser.oneOf
                [ qualifiedOrVariantOrRecordConstructorReferenceExpression
                , unqualifiedFunctionReferenceExpression
                , literalExpression
                , numberExpression
                , tupledExpression
                , Tokens.squareStart |> CustomParser.Extra.continueWith expressionAfterOpeningSquareBracket
                , recordExpression
                , caseExpression
                , lambdaExpression
                , letExpression
                , ifBlockExpression
                , recordAccessFunctionExpression
                , negationOperation
                , charLiteralExpression
                ]
            )


andThenOneOf : List ( Int, Parser (WithComments ExtensionRight) )
andThenOneOf =
    -- TODO Add tests for all operators
    -- TODO Report a syntax error when encountering multiple of the comparison operators
    -- `a < b < c` is not valid Elm syntax
    [ recordAccess
    , infixLeft 1 (CustomParser.lazy (\() -> abovePrecedence1)) "|>"
    , infixRight 5 (CustomParser.lazy (\() -> abovePrecedence4)) "++"
    , infixRight 1 (CustomParser.lazy (\() -> abovePrecedence0)) "<|"
    , infixRight 9 (CustomParser.lazy (\() -> abovePrecedence8)) ">>"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "=="
    , infixLeft 7 (CustomParser.lazy (\() -> abovePrecedence7)) "*"
    , infixRight 5 (CustomParser.lazy (\() -> abovePrecedence4)) "::"
    , infixLeft 6 (CustomParser.lazy (\() -> abovePrecedence6)) "+"
    , infixLeftSubtraction 6 (CustomParser.lazy (\() -> abovePrecedence6))
    , infixLeft 6 (CustomParser.lazy (\() -> abovePrecedence6)) "|."
    , infixRight 3 (CustomParser.lazy (\() -> abovePrecedence2)) "&&"
    , infixLeft 5 (CustomParser.lazy (\() -> abovePrecedence5)) "|="
    , infixLeft 9 (CustomParser.lazy (\() -> abovePrecedence9)) "<<"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "/="
    , infixLeft 7 (CustomParser.lazy (\() -> abovePrecedence7)) "//"
    , infixLeft 7 (CustomParser.lazy (\() -> abovePrecedence7)) "/"
    , infixRight 7 (CustomParser.lazy (\() -> abovePrecedence6)) "</>"
    , infixRight 2 (CustomParser.lazy (\() -> abovePrecedence1)) "||"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "<="
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) ">="
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) ">"
    , infixLeft 8 (CustomParser.lazy (\() -> abovePrecedence8)) "<?>"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "<"
    , infixRight 8 (CustomParser.lazy (\() -> abovePrecedence7)) "^"

    -- function application must be last
    -- TODO validate function application arguments (issue #209)
    , functionCall
    ]


expression : Parser (WithComments (Node Expression))
expression =
    subExpressionMap identity abovePrecedence0


recordAccess : ( Int, Parser (WithComments ExtensionRight) )
recordAccess =
    postfix 100
        recordAccessParser
        ExtendRightByRecordAccess


recordAccessParser : Parser (Node String)
recordAccessParser =
    lookBehindOneCharacter
        |> CustomParser.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    problemRecordAccessStartingWithSpace

                else
                    dotField
            )


problemRecordAccessStartingWithSpace : CustomParser.Parser a
problemRecordAccessStartingWithSpace =
    CustomParser.problem "Record access can't start with a space"


dotField : CustomParser.Parser (Node String)
dotField =
    CustomParser.map
        (\() ->
            \( nameStartRow, nameStartColumn ) ->
                \name ->
                    Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                        name
        )
        Tokens.dot
        |> CustomParser.keep CustomParser.getPosition
        |> CustomParser.keep Tokens.functionName


functionCall : ( Int, Parser (WithComments ExtensionRight) )
functionCall =
    infixHelp 90
        (CustomParser.lazy (\() -> abovePrecedence90))
        (Layout.positivelyIndented ())
        ExtendRightByApplication


glslExpressionAfterOpeningSquareBracket : Parser { comments : Comments, end : Location, expression : Expression }
glslExpressionAfterOpeningSquareBracket =
    CustomParser.map
        (\() ->
            \s ->
                \( endRow, endColumn ) ->
                    { comments = Rope.empty
                    , -- TODO for v8: don't include glslEnd in range
                      end = { row = endRow, column = endColumn + glslEndSymbolLength }
                    , expression = GLSLExpression s
                    }
        )
        (CustomParser.symbol "glsl|")
        |> CustomParser.keep
            (CustomParser.Advanced.loop "" untilGlslEnd)
        |> CustomParser.keep CustomParser.getPosition


glslEndSymbolLength : Int
glslEndSymbolLength =
    String.length glslEndSymbol


glslEndSymbol : String
glslEndSymbol =
    "|]"


untilGlslEnd : String -> Parser (CustomParser.Advanced.Step String String)
untilGlslEnd soFar =
    CustomParser.oneOf
        [ CustomParser.map (\() -> CustomParser.Advanced.Done soFar)
            (CustomParser.symbol glslEndSymbol)
        , CustomParser.mapChompedString
            (\beforeVerticalBar () ->
                CustomParser.Advanced.Loop (soFar ++ beforeVerticalBar)
            )
            (CustomParser.chompWhile (\c -> c /= '|'))
        ]


expressionAfterOpeningSquareBracket : Parser { comments : Comments, end : Location, expression : Expression }
expressionAfterOpeningSquareBracket =
    CustomParser.oneOf
        [ glslExpressionAfterOpeningSquareBracket
        , CustomParser.map
            (\commentsBefore ->
                \maybeElements ->
                    \( endRow, endColumn ) ->
                        case maybeElements of
                            Nothing ->
                                { comments = commentsBefore
                                , end = { row = endRow, column = endColumn }
                                , expression = expressionListEmpty
                                }

                            Just elements ->
                                { comments = commentsBefore |> Rope.prependTo elements.comments
                                , end = { row = endRow, column = endColumn }
                                , expression = ListExpr elements.syntax
                                }
            )
            Layout.maybeLayout
            |> CustomParser.keep
                (CustomParser.oneOf
                    [ CustomParser.map (\() -> Nothing) Tokens.squareEnd
                    , CustomParser.map
                        (\head ->
                            \commentsAfterHead ->
                                \tail ->
                                    Just
                                        { comments =
                                            head.comments
                                                |> Rope.prependTo commentsAfterHead
                                                |> Rope.prependTo tail.comments
                                        , syntax = head.syntax :: tail.syntax
                                        }
                        )
                        expression
                        |> CustomParser.keep Layout.maybeLayout
                        |> CustomParser.keep
                            (ParserWithComments.many
                                (Tokens.comma
                                    |> CustomParser.Extra.continueWith (Layout.maybeAroundBothSides expression)
                                )
                            )
                        |> CustomParser.ignore Tokens.squareEnd
                    ]
                )
            |> CustomParser.keep CustomParser.getPosition
        ]


expressionListEmpty : Expression
expressionListEmpty =
    ListExpr []



-- recordExpression


recordExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordExpression =
    CustomParser.map
        (\() ->
            \commentsBefore ->
                \afterCurly ->
                    \( endRow, endColumn ) ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo afterCurly.comments
                        , end = { row = endRow, column = endColumn }
                        , expression = afterCurly.syntax
                        }
        )
        Tokens.curlyStart
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep recordContentsCurlyEnd
        |> CustomParser.keep CustomParser.getPosition


expressionRecordEmptyWithComments : WithComments Expression
expressionRecordEmptyWithComments =
    { comments = Rope.empty, syntax = RecordExpr [] }


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    CustomParser.oneOf
        [ CustomParser.map
            (\( nameStartRow, nameStartColumn ) ->
                \name ->
                    \commentsAfterFunctionName ->
                        \afterNameBeforeFields ->
                            \tailFields ->
                                \commentsBeforeClosingCurly ->
                                    let
                                        nameNode : Node String
                                        nameNode =
                                            Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                name
                                    in
                                    { comments =
                                        commentsAfterFunctionName
                                            |> Rope.prependTo afterNameBeforeFields.comments
                                            |> Rope.prependTo tailFields.comments
                                            |> Rope.prependTo commentsBeforeClosingCurly
                                    , syntax =
                                        case afterNameBeforeFields.syntax of
                                            RecordUpdateFirstSetter firstField ->
                                                RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                                            FieldsFirstValue firstFieldValue ->
                                                RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                                    }
            )
            CustomParser.getPosition
            |> CustomParser.keep Tokens.functionName
            |> CustomParser.keep Layout.maybeLayout
            |> CustomParser.keep
                (CustomParser.oneOf
                    [ CustomParser.map
                        (\() ->
                            \commentsBefore ->
                                \setterResult ->
                                    { comments = commentsBefore |> Rope.prependTo setterResult.comments
                                    , syntax = RecordUpdateFirstSetter setterResult.syntax
                                    }
                        )
                        Tokens.pipe
                        |> CustomParser.keep Layout.maybeLayout
                        |> CustomParser.keep recordSetterNodeWithLayout
                    , CustomParser.map
                        (\() ->
                            \commentsBefore ->
                                \expressionResult ->
                                    \commentsAfter ->
                                        { comments =
                                            commentsBefore
                                                |> Rope.prependTo expressionResult.comments
                                                |> Rope.prependTo commentsAfter
                                        , syntax = FieldsFirstValue expressionResult.syntax
                                        }
                        )
                        Tokens.equal
                        |> CustomParser.keep Layout.maybeLayout
                        |> CustomParser.keep expression
                        |> CustomParser.keep Layout.maybeLayout
                    ]
                )
            |> CustomParser.keep recordFields
            |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token "}")
        , CustomParser.map (\() -> expressionRecordEmptyWithComments) Tokens.curlyEnd
        ]


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        ((CustomParser.map
            (\() ->
                \commentsBefore ->
                    \setterResult ->
                        { comments = commentsBefore |> Rope.prependTo setterResult.comments
                        , syntax = setterResult.syntax
                        }
            )
            Tokens.comma
            |> CustomParser.keep Layout.maybeLayout
         )
            |> CustomParser.keep recordSetterNodeWithLayout
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    CustomParser.map
        (\( nameStartRow, nameStartColumn ) ->
            \name ->
                \commentsAfterFunctionName ->
                    \commentsAfterEquals ->
                        \expressionResult ->
                            \commentsAfterExpression ->
                                \( endRow, endColumn ) ->
                                    let
                                        start : Location
                                        start =
                                            { row = nameStartRow, column = nameStartColumn }
                                    in
                                    { comments =
                                        commentsAfterFunctionName
                                            |> Rope.prependTo commentsAfterEquals
                                            |> Rope.prependTo expressionResult.comments
                                            |> Rope.prependTo commentsAfterExpression
                                    , syntax =
                                        Node { start = start, end = { row = endRow, column = endColumn } }
                                            ( Node.singleLineStringFrom start
                                                name
                                            , expressionResult.syntax
                                            )
                                    }
        )
        CustomParser.getPosition
        |> CustomParser.keep Tokens.functionName
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token "=")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep Layout.maybeLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove and use (Node.range expr).end
        |> CustomParser.keep CustomParser.getPosition


literalExpression : Parser { comments : Comments, end : Location, expression : Expression }
literalExpression =
    CustomParser.map
        (\string ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Literal string
                }
        )
        Tokens.singleOrTripleQuotedStringLiteral
        |> CustomParser.keep CustomParser.getPosition


charLiteralExpression : Parser { comments : Comments, end : Location, expression : Expression }
charLiteralExpression =
    CustomParser.map
        (\char ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = CharLiteral char
                }
        )
        Tokens.characterLiteral
        |> CustomParser.keep CustomParser.getPosition



-- lambda


lambdaExpression : Parser { comments : Comments, end : Location, expression : Expression }
lambdaExpression =
    CustomParser.map
        (\() ->
            \commentsAfterBackslash ->
                \firstArg ->
                    \commentsAfterFirstArg ->
                        \secondUpArgs ->
                            \expressionResult ->
                                let
                                    (Node { end } _) =
                                        expressionResult.syntax
                                in
                                { comments =
                                    commentsAfterBackslash
                                        |> Rope.prependTo firstArg.comments
                                        |> Rope.prependTo commentsAfterFirstArg
                                        |> Rope.prependTo secondUpArgs.comments
                                        |> Rope.prependTo expressionResult.comments
                                , end = end
                                , expression =
                                    { args = firstArg.syntax :: secondUpArgs.syntax
                                    , expression = expressionResult.syntax
                                    }
                                        |> LambdaExpression
                                }
        )
        Tokens.backSlash
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep Patterns.pattern
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep
            (ParserWithComments.until
                Tokens.arrowRight
                (CustomParser.map
                    (\patternResult ->
                        \commentsAfter ->
                            { comments =
                                patternResult.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax = patternResult.syntax
                            }
                    )
                    Patterns.pattern
                    |> CustomParser.keep Layout.maybeLayout
                )
            )
        |> CustomParser.keep expression



-- Case Expression


caseExpression : Parser { comments : Comments, end : Location, expression : Expression }
caseExpression =
    CustomParser.map
        (\() ->
            \commentsAfterCase ->
                \casedExpressionResult ->
                    \commentsBeforeOf ->
                        \commentsAfterOf ->
                            \casesResult ->
                                let
                                    ( ( _, Node firstCaseExpressionRange _ ) as firstCase, lastToSecondCase ) =
                                        casesResult.syntax
                                in
                                { comments =
                                    commentsAfterCase
                                        |> Rope.prependTo casedExpressionResult.comments
                                        |> Rope.prependTo commentsBeforeOf
                                        |> Rope.prependTo commentsAfterOf
                                        |> Rope.prependTo casesResult.comments
                                , end =
                                    case lastToSecondCase of
                                        [] ->
                                            firstCaseExpressionRange.end

                                        ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                            lastCaseExpressionRange.end
                                , expression =
                                    CaseExpression
                                        { expression = casedExpressionResult.syntax
                                        , cases = firstCase :: List.reverse lastToSecondCase
                                        }
                                }
        )
        Tokens.caseToken
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.keyword "of")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep (CustomParser.Extra.withIndent caseStatements)


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    CustomParser.map
        (\firstCasePatternResult ->
            \commentsAfterFirstCasePattern ->
                \commentsAfterFirstCaseArrowRight ->
                    \firstCaseExpressionResult ->
                        \lastToSecondCase ->
                            { comments =
                                firstCasePatternResult.comments
                                    |> Rope.prependTo commentsAfterFirstCasePattern
                                    |> Rope.prependTo commentsAfterFirstCaseArrowRight
                                    |> Rope.prependTo firstCaseExpressionResult.comments
                                    |> Rope.prependTo lastToSecondCase.comments
                            , syntax =
                                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                                , lastToSecondCase.syntax
                                )
                            }
        )
        Patterns.pattern
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token "->")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep (ParserWithComments.manyWithoutReverse caseStatement)


caseStatement : Parser (WithComments Case)
caseStatement =
    CustomParser.map
        (\commentsBeforeCase ->
            \pattern ->
                \commentsBeforeArrowRight ->
                    \commentsAfterArrowRight ->
                        \expr ->
                            { comments =
                                commentsBeforeCase
                                    |> Rope.prependTo pattern.comments
                                    |> Rope.prependTo commentsBeforeArrowRight
                                    |> Rope.prependTo commentsAfterArrowRight
                                    |> Rope.prependTo expr.comments
                            , syntax = ( pattern.syntax, expr.syntax )
                            }
        )
        (Layout.optimisticLayout |> CustomParser.backtrackable)
        |> CustomParser.ignore (Layout.onTopIndentation ())
        |> CustomParser.keep Patterns.pattern
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token "->")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression



-- Let Expression


letExpression : Parser { comments : Comments, end : Location, expression : Expression }
letExpression =
    CustomParser.Extra.withIndent
        ((CustomParser.map
            (\() ->
                \commentsAfterLet ->
                    \declarations ->
                        \commentsAfterIn ->
                            \expressionResult ->
                                let
                                    ((Node { end } _) as expr) =
                                        expressionResult.syntax
                                in
                                { comments =
                                    commentsAfterLet
                                        |> Rope.prependTo declarations.comments
                                        |> Rope.prependTo commentsAfterIn
                                        |> Rope.prependTo expressionResult.comments
                                , end = end
                                , expression = LetExpression { declarations = declarations.syntax, expression = expr }
                                }
            )
            Tokens.letToken
            |> CustomParser.keep Layout.maybeLayout
         )
            |> CustomParser.keep (CustomParser.Extra.withIndent letDeclarationsIn)
        )
        -- check that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        |> CustomParser.ignore (Layout.positivelyIndentedPlus 2)
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    Layout.onTopIndentation
        (\headLetResult ->
            \commentsAfter ->
                \tailLetResult ->
                    { comments =
                        headLetResult.comments
                            |> Rope.prependTo commentsAfter
                            |> Rope.prependTo tailLetResult.comments
                    , syntax = headLetResult.syntax :: tailLetResult.syntax
                    }
        )
        |> CustomParser.keep
            (CustomParser.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )
        |> CustomParser.keep Layout.optimisticLayout
        |> CustomParser.keep (ParserWithComments.until Tokens.inToken blockElement)


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentation
        (\letDeclarationResult ->
            \commentsAfter ->
                { comments = letDeclarationResult.comments |> Rope.prependTo commentsAfter
                , syntax = letDeclarationResult.syntax
                }
        )
        |> CustomParser.keep
            (CustomParser.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )
        |> CustomParser.keep Layout.optimisticLayout


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    CustomParser.map
        (\pattern ->
            \expressionResult ->
                let
                    (Node { start } _) =
                        pattern.syntax

                    (Node { end } _) =
                        expressionResult.syntax
                in
                { comments = pattern.comments |> Rope.prependTo expressionResult.comments
                , syntax =
                    Node { start = start, end = end }
                        (LetDestructuring pattern.syntax expressionResult.syntax)
                }
        )
        Patterns.pattern
        |> CustomParser.ignore Tokens.equal
        |> CustomParser.keep expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    CustomParser.map
        (\( startNameStartRow, startNameStartColumn ) ->
            \startName ->
                \commentsAfterStartName ->
                    \maybeSignature ->
                        \arguments ->
                            \commentsAfterEqual ->
                                \expressionResult ->
                                    let
                                        allComments : Comments
                                        allComments =
                                            commentsAfterStartName
                                                |> Rope.prependTo
                                                    (case maybeSignature of
                                                        Nothing ->
                                                            Rope.empty

                                                        Just signature ->
                                                            signature.comments
                                                    )
                                                |> Rope.prependTo arguments.comments
                                                |> Rope.prependTo commentsAfterEqual
                                                |> Rope.prependTo expressionResult.comments

                                        start : Location
                                        start =
                                            { row = startNameStartRow, column = startNameStartColumn }

                                        startNameNode : Node String
                                        startNameNode =
                                            Node.singleLineStringFrom start
                                                startName
                                    in
                                    case maybeSignature of
                                        Nothing ->
                                            let
                                                (Node expressionRange _) =
                                                    expressionResult.syntax
                                            in
                                            { comments = allComments
                                            , syntax =
                                                Node { start = start, end = expressionRange.end }
                                                    (LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            Node { start = start, end = expressionRange.end }
                                                                { name = startNameNode
                                                                , arguments = arguments.syntax
                                                                , expression = expressionResult.syntax
                                                                }
                                                        }
                                                    )
                                            }
                                                |> CustomParser.succeed

                                        Just signature ->
                                            let
                                                (Node implementationNameRange implementationName) =
                                                    signature.implementationName
                                            in
                                            if implementationName == startName then
                                                let
                                                    (Node expressionRange _) =
                                                        expressionResult.syntax
                                                in
                                                { comments = allComments
                                                , syntax =
                                                    Node { start = start, end = expressionRange.end }
                                                        (LetFunction
                                                            { documentation = Nothing
                                                            , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                                            , declaration =
                                                                Node { start = implementationNameRange.start, end = expressionRange.end }
                                                                    { name = signature.implementationName
                                                                    , arguments = arguments.syntax
                                                                    , expression = expressionResult.syntax
                                                                    }
                                                            }
                                                        )
                                                }
                                                    |> CustomParser.succeed

                                            else
                                                CustomParser.problem
                                                    ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
        )
        CustomParser.getPosition
        |> CustomParser.keep Tokens.functionName
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\() ->
                        \commentsBeforeTypeAnnotation ->
                            \typeAnnotationResult ->
                                \commentsAfterTypeAnnotation ->
                                    \( implementationNameStartRow, implementationNameStartColumn ) ->
                                        \implementationName ->
                                            \afterImplementationName ->
                                                Just
                                                    { comments =
                                                        commentsBeforeTypeAnnotation
                                                            |> Rope.prependTo typeAnnotationResult.comments
                                                            |> Rope.prependTo commentsAfterTypeAnnotation
                                                            |> Rope.prependTo afterImplementationName
                                                    , implementationName =
                                                        Node.singleLineStringFrom { row = implementationNameStartRow, column = implementationNameStartColumn }
                                                            implementationName
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                    )
                    Tokens.colon
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep TypeAnnotation.typeAnnotation
                    |> CustomParser.keep Layout.layoutStrict
                    |> CustomParser.keep CustomParser.getPosition
                    |> CustomParser.keep Tokens.functionName
                    |> CustomParser.keep Layout.maybeLayout
                , CustomParser.succeed Nothing
                ]
            )
        |> CustomParser.keep parameterPatternsEqual
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.andThen identity


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (CustomParser.map
            (\patternResult ->
                \commentsAfterPattern ->
                    { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                    , syntax = patternResult.syntax
                    }
            )
            Patterns.pattern
            |> CustomParser.keep Layout.maybeLayout
        )


numberExpression : Parser { comments : Comments, end : Location, expression : Expression }
numberExpression =
    Elm.Parser.Numbers.forgivingNumber
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Floatable n
                }
        )
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Integer n
                }
        )
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Hex n
                }
        )
        |> CustomParser.keep CustomParser.getPosition


ifBlockExpression : Parser { comments : Comments, end : Location, expression : Expression }
ifBlockExpression =
    CustomParser.map
        (\() ->
            \commentsAfterIf ->
                \condition ->
                    \commentsBeforeThen ->
                        \commentsAfterThen ->
                            \ifTrue ->
                                \commentsBeforeElse ->
                                    \commentsAfterElse ->
                                        \ifFalse ->
                                            let
                                                (Node { end } _) =
                                                    ifFalse.syntax
                                            in
                                            { comments =
                                                commentsAfterIf
                                                    |> Rope.prependTo condition.comments
                                                    |> Rope.prependTo commentsBeforeThen
                                                    |> Rope.prependTo commentsAfterThen
                                                    |> Rope.prependTo ifTrue.comments
                                                    |> Rope.prependTo commentsBeforeElse
                                                    |> Rope.prependTo commentsAfterElse
                                                    |> Rope.prependTo ifFalse.comments
                                            , end = end
                                            , expression = IfBlock condition.syntax ifTrue.syntax ifFalse.syntax
                                            }
        )
        Tokens.ifToken
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.keyword "then")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.keyword "else")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression


negationOperation : Parser { comments : Comments, end : Location, expression : Expression }
negationOperation =
    minusNotFollowedBySpace
        |> CustomParser.Extra.continueWith
            (subExpressionMap
                (\subExpressionResult ->
                    let
                        (Node { end } _) =
                            subExpressionResult.syntax
                    in
                    { comments = subExpressionResult.comments
                    , end = end
                    , expression = Negation subExpressionResult.syntax
                    }
                )
                abovePrecedence95
            )


minusNotFollowedBySpace : CustomParser.Parser ()
minusNotFollowedBySpace =
    Tokens.minus
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                [ CustomParser.chompIf (\next -> next == '\u{000D}' || next == '\n' || next == ' ')
                    |> CustomParser.Extra.continueWith (CustomParser.problem "negation sign cannot be followed by a space")
                , CustomParser.succeed ()
                ]
            )
        |> CustomParser.backtrackable


qualifiedOrVariantOrRecordConstructorReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
qualifiedOrVariantOrRecordConstructorReferenceExpression =
    CustomParser.map
        (\firstName ->
            \after ->
                \( endRow, endColumn ) ->
                    { comments = Rope.empty
                    , end = { row = endRow, column = endColumn }
                    , expression =
                        case after of
                            Nothing ->
                                FunctionOrValue [] firstName

                            Just ( qualificationAfter, unqualified ) ->
                                FunctionOrValue (firstName :: qualificationAfter) unqualified
                    }
        )
        Tokens.typeName
        |> CustomParser.keep maybeDotReferenceExpressionTuple
        |> CustomParser.keep CustomParser.getPosition


unqualifiedFunctionReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
unqualifiedFunctionReferenceExpression =
    CustomParser.map
        (\unqualified ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = FunctionOrValue [] unqualified
                }
        )
        Tokens.functionName
        |> CustomParser.keep CustomParser.getPosition


maybeDotReferenceExpressionTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    CustomParser.oneOf
        [ Tokens.dot
            |> CustomParser.Extra.continueWith
                (CustomParser.oneOf
                    [ CustomParser.map
                        (\firstName ->
                            \after ->
                                Just
                                    (case after of
                                        Nothing ->
                                            ( [], firstName )

                                        Just ( qualificationAfter, unqualified ) ->
                                            ( firstName :: qualificationAfter, unqualified )
                                    )
                        )
                        Tokens.typeName
                        |> CustomParser.keep (CustomParser.lazy (\() -> maybeDotReferenceExpressionTuple))
                    , CustomParser.map (\unqualified -> Just ( [], unqualified )) Tokens.functionName
                    ]
                )
        , CustomParser.succeed Nothing
        ]


recordAccessFunctionExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordAccessFunctionExpression =
    CustomParser.map
        (\() ->
            \field ->
                \( endRow, endColumn ) ->
                    { comments = Rope.empty
                    , end = { row = endRow, column = endColumn }
                    , expression = RecordAccessFunction ("." ++ field)
                    }
        )
        Tokens.dot
        |> CustomParser.keep Tokens.functionName
        |> CustomParser.keep CustomParser.getPosition


tupledExpression : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpression =
    Tokens.parensStart
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                ((CustomParser.map
                    (\() ->
                        \( endRow, endColumn ) ->
                            { comments = Rope.empty
                            , end = { row = endRow, column = endColumn }
                            , expression = UnitExpr
                            }
                    )
                    Tokens.parensEnd
                    |> CustomParser.keep CustomParser.getPosition
                 )
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       (CustomParser.map
                            (\() ->
                                \( endRow, endColumn ) ->
                                    { comments = Rope.empty
                                    , end = { row = endRow, column = endColumn }
                                    , expression = expressionPrefixOperatorMinus
                                    }
                            )
                            (CustomParser.symbol "-)")
                            |> CustomParser.keep CustomParser.getPosition
                       )
                    :: tupledExpressionInnerAfterOpeningParens
                    -- and since prefix operators are much more rare than e.g. parenthesized
                    -- we check those later
                    :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
                )
            )


expressionPrefixOperatorMinus : Expression
expressionPrefixOperatorMinus =
    PrefixOperator "-"


allowedPrefixOperatorExceptMinusThenClosingParensOneOf : List (Parser { comments : Comments, end : Location, expression : Expression })
allowedPrefixOperatorExceptMinusThenClosingParensOneOf =
    Tokens.allowedOperatorTokens
        |> List.filter (\token -> token /= "-")
        |> List.map
            (\allowedOperatorToken ->
                CustomParser.map
                    (\() ->
                        \( endRow, endColumn ) ->
                            { comments = Rope.empty
                            , end = { row = endRow, column = endColumn }
                            , expression = PrefixOperator allowedOperatorToken
                            }
                    )
                    (CustomParser.symbol (allowedOperatorToken ++ ")"))
                    |> CustomParser.keep CustomParser.getPosition
            )


tupledExpressionInnerAfterOpeningParens : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpressionInnerAfterOpeningParens =
    CustomParser.map
        (\firstPart ->
            \commentsAfterFirstPart ->
                \tailPartsReverse ->
                    \( endRow, endColumn ) ->
                        case tailPartsReverse.syntax of
                            [] ->
                                { comments = firstPart.comments |> Rope.prependTo commentsAfterFirstPart
                                , end = { row = endRow, column = endColumn }
                                , expression = ParenthesizedExpression firstPart.syntax
                                }

                            _ ->
                                { comments = firstPart.comments |> Rope.prependTo tailPartsReverse.comments
                                , end = { row = endRow, column = endColumn }
                                , expression = TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                                }
        )
        expression
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep
            (ParserWithComments.untilWithoutReverse
                Tokens.parensEnd
                (CustomParser.map
                    (\() ->
                        \commentsBefore ->
                            \partResult ->
                                \commentsAfter ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo partResult.comments
                                            |> Rope.prependTo commentsAfter
                                    , syntax = partResult.syntax
                                    }
                    )
                    Tokens.comma
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep expression
                    |> CustomParser.keep Layout.maybeLayout
                )
            )
        |> CustomParser.keep CustomParser.getPosition



---


subExpressionMap :
    (WithComments (Node Expression) -> a)
    -> Parser (WithComments ExtensionRight)
    -> Parser a
subExpressionMap toExtensionRightWith aboveCurrentPrecedenceLayout =
    let
        step :
            WithComments (Node Expression)
            -> Parser (CustomParser.Advanced.Step (WithComments (Node Expression)) a)
        step leftExpressionResult =
            CustomParser.oneOf
                [ CustomParser.map
                    (\extensionRight ->
                        \commentsAfter ->
                            { comments =
                                leftExpressionResult.comments
                                    |> Rope.prependTo extensionRight.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax =
                                leftExpressionResult.syntax
                                    |> applyExtensionRight extensionRight.syntax
                            }
                                |> CustomParser.Advanced.Loop
                    )
                    aboveCurrentPrecedenceLayout
                    |> CustomParser.keep Layout.optimisticLayout
                , CustomParser.succeed
                    (CustomParser.Advanced.Done (toExtensionRightWith leftExpressionResult))
                ]
    in
    CustomParser.map
        (\commentsBefore ->
            \leftExpressionResult ->
                \commentsAfter ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo leftExpressionResult.comments
                            |> Rope.prependTo commentsAfter
                    , syntax = leftExpressionResult.syntax
                    }
        )
        Layout.optimisticLayout
        |> CustomParser.keep (CustomParser.lazy (\() -> subExpression))
        |> CustomParser.keep Layout.optimisticLayout
        |> CustomParser.andThen
            (\leftExpression -> CustomParser.Advanced.loop leftExpression step)


applyExtensionRight : ExtensionRight -> Node Expression -> Node Expression
applyExtensionRight extensionRight ((Node { start } left) as leftNode) =
    case extensionRight of
        ExtendRightByRecordAccess ((Node { end } _) as field) ->
            Node { start = start, end = end }
                (Expression.RecordAccess leftNode field)

        ExtendRightByApplication ((Node { end } _) as right) ->
            Node { start = start, end = end }
                (Expression.Application
                    (case left of
                        Expression.Application (called :: firstArg :: secondArgUp) ->
                            called :: firstArg :: (secondArgUp ++ [ right ])

                        _ ->
                            [ leftNode, right ]
                    )
                )

        ExtendRightByOperation extendRightOperation ->
            let
                ((Node { end } _) as right) =
                    extendRightOperation.expression
            in
            Node { start = start, end = end }
                (OperatorApplication extendRightOperation.symbol extendRightOperation.direction leftNode right)


abovePrecedence0 : Parser (WithComments ExtensionRight)
abovePrecedence0 =
    computeAbovePrecedence 0


abovePrecedence1 : Parser (WithComments ExtensionRight)
abovePrecedence1 =
    computeAbovePrecedence 1


abovePrecedence2 : Parser (WithComments ExtensionRight)
abovePrecedence2 =
    computeAbovePrecedence 2


abovePrecedence4 : Parser (WithComments ExtensionRight)
abovePrecedence4 =
    computeAbovePrecedence 4


abovePrecedence5 : Parser (WithComments ExtensionRight)
abovePrecedence5 =
    computeAbovePrecedence 5


abovePrecedence6 : Parser (WithComments ExtensionRight)
abovePrecedence6 =
    computeAbovePrecedence 6


abovePrecedence7 : Parser (WithComments ExtensionRight)
abovePrecedence7 =
    computeAbovePrecedence 7


abovePrecedence8 : Parser (WithComments ExtensionRight)
abovePrecedence8 =
    computeAbovePrecedence 8


abovePrecedence9 : Parser (WithComments ExtensionRight)
abovePrecedence9 =
    computeAbovePrecedence 9


abovePrecedence90 : Parser (WithComments ExtensionRight)
abovePrecedence90 =
    computeAbovePrecedence 90


abovePrecedence95 : Parser (WithComments ExtensionRight)
abovePrecedence95 =
    computeAbovePrecedence 95


computeAbovePrecedence : Int -> Parser (WithComments ExtensionRight)
computeAbovePrecedence currentPrecedence =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just parser

                else
                    Nothing
            )
        |> CustomParser.oneOf


infixLeft : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixLeft precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (CustomParser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Left, expression = right }
        )


infixNonAssociative : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixNonAssociative precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (CustomParser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Non, expression = right }
        )


{-| To get right associativity, please provide abovePrecedence(precedence-1) for the
right precedence parser.
-}
infixRight : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixRight precedence possibilitiesForPrecedenceMinus1 symbol =
    infixHelp precedence
        possibilitiesForPrecedenceMinus1
        (CustomParser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Right, expression = right }
        )


lookBehindOneCharacter : CustomParser.Parser String
lookBehindOneCharacter =
    CustomParser.map (\offset -> \source -> String.slice (offset - 1) offset source)
        CustomParser.getOffset
        |> CustomParser.keep CustomParser.getSource


infixLeftSubtraction : Int -> Parser (WithComments ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
infixLeftSubtraction precedence possibilitiesForPrecedence =
    infixHelp precedence
        possibilitiesForPrecedence
        (lookBehindOneCharacter
            |> CustomParser.andThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        Tokens.minusSymbols

                    else
                        Tokens.minus
                )
        )
        (\right ->
            ExtendRightByOperation { symbol = "-", direction = Infix.Left, expression = right }
        )


infixHelp :
    Int
    -> Parser (WithComments ExtensionRight)
    -> CustomParser.Parser ()
    -> (Node Expression -> ExtensionRight)
    -> ( Int, Parser (WithComments ExtensionRight) )
infixHelp leftPrecedence rightPrecedence operator apply =
    ( leftPrecedence
    , operator
        |> CustomParser.Extra.continueWith
            (subExpressionMap
                (\e ->
                    { comments = e.comments
                    , syntax = apply e.syntax
                    }
                )
                rightPrecedence
            )
    )


postfix : Int -> Parser a -> (a -> ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
postfix precedence operator apply =
    ( precedence
    , CustomParser.map
        (\right ->
            { comments = Rope.empty
            , syntax = apply right
            }
        )
        operator
    )


type ExtensionRight
    = ExtendRightByOperation { symbol : String, direction : Infix.InfixDirection, expression : Node Expression }
    | ExtendRightByApplication (Node Expression)
    | ExtendRightByRecordAccess (Node String)
