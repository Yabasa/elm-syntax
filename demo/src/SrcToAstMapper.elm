module SrcToAstMapper exposing (SrcToAstMapping, mapSrcToAst)

import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing (..)


{-| Given a piece of source code and it's parsed AST as a string, determine the
mappings between them such that a start and end offset in the source code string
maps to the start and end offsets in the AST string which represent that section
of the source code string.
-}
mapSrcToAst : String -> String -> List SrcToAstMapping
mapSrcToAst srcString astString =
    run pNodeTrees astString
        |> Result.map (List.map (createMapping srcString))
        |> Result.withDefault []



--
-- TYPES
--


{-| Intermediate type for holding raw parsed AST info.

Payloads are essentially:
AstStringOffsetStart SrcRange AstStringOffsetEnd

-}
type AstNodeInfo
    = AstNodeInfo Int Range Int


{-| Final type for holding mapping with src range converted to offsets for more
convenient processing later.
-}
type alias SrcToAstMapping =
    { srcStringOffsetStart : Int
    , srcStringOffsetEnd : Int
    , astStringOffsetStart : Int
    , astStringOffsetEnd : Int
    }



--
-- AST STRING PARSERS
--


pLoc : Parser Location
pLoc =
    succeed (\col row -> Location row col)
        |. symbol "{"
        |. spaces
        |. keyword "column"
        |. spaces
        |. symbol "="
        |. spaces
        |= int
        |. spaces
        |. symbol ","
        |. spaces
        |. keyword "row"
        |. spaces
        |. symbol "="
        |. spaces
        |= int
        |. spaces
        |. symbol "}"


pRange : Parser Range
pRange =
    succeed (\end start -> Range start end)
        |. symbol "{"
        |. spaces
        |. keyword "end"
        |. spaces
        |. symbol "="
        |. spaces
        |= pLoc
        |. spaces
        |. symbol ","
        |. spaces
        |. keyword "start"
        |. spaces
        |. symbol "="
        |. spaces
        |= pLoc
        |. spaces
        |. symbol "}"


{-| A Node type contains a Range and a value that can contain more nodes hence
it's described here as a node tree. All nested Nodes are surfaced up to their
parent to be combined into a flat list.
-}
pNodeTree : Parser (List AstNodeInfo)
pNodeTree =
    succeed
        (\aso sr nn aeo -> AstNodeInfo aso sr aeo :: nn)
        |= getOffset
        |. keyword "Node"
        |. spaces
        |= pRange
        |. spaces
        |= pNodeValue
        |= getOffset


{-| There are 4 types of Node values:

1.  a simple quoted string
2.  a tuple or a value wrapped in parentheses
3.  a list inside square brackets
4.  a record inside curly braces

For cases 2, 3 and 4 there may be nested Nodes within.

-}
pNodeValue : Parser (List AstNodeInfo)
pNodeValue =
    oneOf
        [ pQuotedString
        , pContainer
        ]


pQuotedString : Parser (List AstNodeInfo)
pQuotedString =
    succeed []
        |. symbol "\""
        |. chompWhile (\c -> c /= '"')
        |. symbol "\""


pContainer : Parser (List AstNodeInfo)
pContainer =
    oneOf
        [ pNodeTreesBetween "[" "]"
        , pNodeTreesBetween "{" "}"
        , pNodeTreesBetween "(" ")"
        ]


pNodeTreesBetween : String -> String -> Parser (List AstNodeInfo)
pNodeTreesBetween preStr postStr =
    succeed identity
        |. symbol preStr
        |= pNodeTrees
        |. symbol postStr


{-| Skip over characters inbetween Nodes.

Special considerations:

    1. The keyword parser used within pNodeTree can be triggered within any
       sequence of alpha chars that have "Node" at the end because once the
       parser has chomped up to the "N", the keyword parser would then succeed
       so long as there is whitespace after the e. To avoid this situation,
       always skip to the end of any sequence of alpha chars.

    2. The AST custom type "Node" may by chance exist as a custom type in the
       original source code and will mistaken as an actual AST Node. To work
       around this, just skip past all quoted strings because custom types in
       the source will be quoted in the AST.

    3. Never skip over opening or closing chars for tuples, records or lists.
       They will be handled by pNodeTreesBetween.

-}
pSkip : Parser (List AstNodeInfo)
pSkip =
    succeed []
        |. chompWhile Char.isAlpha
        |. oneOf
            [ pQuotedString |> map (\_ -> ())
            , chompIf (\c -> not (List.member c [ '[', ']', '{', '}', '(', ')' ]))
            ]


{-| Parse NodeTrees at the same level of nesting.
-}
pNodeTrees : Parser (List AstNodeInfo)
pNodeTrees =
    loop [] pNodeTreesHelp


pNodeTreesHelp : List AstNodeInfo -> Parser (Step (List AstNodeInfo) (List AstNodeInfo))
pNodeTreesHelp nodeTrees =
    oneOf
        [ succeed (\nodeTree -> Loop (List.append nodeTrees nodeTree))
            |= pNodeTree
        , succeed (\_ -> Loop nodeTrees)
            |= pSkip
        , succeed (\newNodeTrees -> Loop (List.append nodeTrees newNodeTrees))
            |= pContainer
        , succeed ()
            |> map (\_ -> Done (List.reverse nodeTrees))
        ]



--
-- UTILS
--


createMapping : String -> AstNodeInfo -> SrcToAstMapping
createMapping srcString (AstNodeInfo astStart srcRange astEnd) =
    let
        srcStart =
            locToOffset srcRange.start srcString

        srcEnd =
            locToOffset srcRange.end srcString
    in
    { srcStringOffsetStart = srcStart
    , srcStringOffsetEnd = srcEnd
    , astStringOffsetStart = astStart
    , astStringOffsetEnd = astEnd
    }


{-| Count the number of chars from the start of a string for any given row/col.

Don't forget to add 1 when getting the length of each line to account for
newline chars.

-}
locToOffset : Location -> String -> Int
locToOffset loc src =
    String.lines src
        |> List.take (loc.row - 1)
        |> List.map (\s -> String.length s + 1)
        |> List.sum
        |> (+) loc.column
