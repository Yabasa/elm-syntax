module SrcToAstMapper exposing (SrcToAstMapping, mapSrcToAst)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)


{-| Given a piece of source code and it's parsed AST as a string, determine the
mappings between them such that a start and end offset in the source code string
maps to the start and end offsets in the AST string which represent that section
of the source code string.
-}
mapSrcToAst : String -> String -> List SrcToAstMapping
mapSrcToAst srcString astString =
    let
        parsableAstString =
            "module DisplayAst exposing (ast)\nast = " ++ astString

        astAst =
            astOfAstString parsableAstString

        rngs =
            astAst
                |> extractNodeRanges
    in
    List.map (createMapping srcString parsableAstString) rngs



-- This is pulling out the Node that represents the Node in the source string with all the module stuff that was added to enable it to be parsed removed.


astOfAstString : String -> Node Expression
astOfAstString astStr =
    let
        parseResult =
            astStr
                |> Elm.Parser.parse
                |> Result.map (Elm.Processing.process Elm.Processing.init)

        dbg =
            Debug.log "parseResult" parseResult
    in
    Result.map .declarations parseResult
        |> Result.withDefault []
        |> List.head
        |> Maybe.withDefault
            (Node.empty
                (CustomTypeDeclaration
                    { documentation = Nothing
                    , name = Node.empty ""
                    , generics = []
                    , constructors = []
                    }
                )
            )
        |> (\(Node _ d) -> d)
        |> (\dec ->
                case dec of
                    FunctionDeclaration func ->
                        case func.declaration of
                            Node _ fi ->
                                fi.expression

                    _ ->
                        Node.empty (Application [])
           )



--
-- TYPES
--


{-| Intermediate type for holding raw source and AST ranges that map to each toher.
-}
type alias SrcToAstMappingRanges =
    { srcStrRange : Range
    , astStrRange : Range
    }


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
-- AST PROCESSING
--


extractNodeRanges : Node Expression -> List SrcToAstMappingRanges
extractNodeRanges node =
    extractNodeRangesHelp [] node


extractNodeRangesHelp : List SrcToAstMappingRanges -> Node Expression -> List SrcToAstMappingRanges
extractNodeRangesHelp nodeRngs (Node astStrRng expr) =
    case expr of
        Application (name :: rng :: payload) ->
            case name of
                Node _ (FunctionOrValue [] "Node") ->
                    let
                        thisNodeInfo =
                            { srcStrRange = extractSrcRange rng
                            , astStrRange = astStrRng
                            }

                        payloadNodeInfos =
                            List.map (extractNodeRangesHelp []) payload
                                |> List.concat
                    in
                    (thisNodeInfo :: nodeRngs) ++ payloadNodeInfos

                _ ->
                    List.map (extractNodeRangesHelp []) (rng :: payload)
                        |> List.concat
                        |> (++) nodeRngs

        ParenthesizedExpression nodeExpr ->
            extractNodeRangesHelp nodeRngs nodeExpr

        RecordExpr recSetters ->
            recSetters
                |> List.map Node.value
                |> List.map Tuple.second
                |> List.map (extractNodeRangesHelp [])
                |> List.concat
                |> (++) nodeRngs

        ListExpr exprs ->
            exprs
                |> List.map (extractNodeRangesHelp [])
                |> List.concat
                |> (++) nodeRngs

        _ ->
            []


extractSrcRange : Node Expression -> Range
extractSrcRange recordAst =
    case recordAst of
        Node _ (RecordExpr (e1 :: e2 :: _)) ->
            let
                ( locType1, loc1 ) =
                    extractLoc e1

                ( _, loc2 ) =
                    extractLoc e2
            in
            case locType1 of
                "start" ->
                    { start = loc1, end = loc2 }

                _ ->
                    { start = loc2, end = loc1 }

        _ ->
            Range.empty


extractLoc : Node ( Node String, Node Expression ) -> ( String, Location )
extractLoc (Node _ ( Node _ key, Node _ expr )) =
    case expr of
        RecordExpr (e1 :: e2 :: _) ->
            let
                ( rowOrColType1, rowOrColVal1 ) =
                    extractRowOrCol e1

                ( _, rowOrColVal2 ) =
                    extractRowOrCol e2
            in
            case rowOrColType1 of
                "row" ->
                    ( key, { row = rowOrColVal1, column = rowOrColVal2 } )

                _ ->
                    ( key, { row = rowOrColVal2, column = rowOrColVal1 } )

        _ ->
            ( "", { row = 0, column = 0 } )


extractRowOrCol rowOrCol =
    case rowOrCol of
        Node _ ( Node _ locType, Node _ (Integer int) ) ->
            ( locType, int )

        _ ->
            ( "", 0 )



--
-- UTILS
--


createMapping : String -> String -> SrcToAstMappingRanges -> SrcToAstMapping
createMapping srcString astString { srcStrRange, astStrRange } =
    let
        srcStart =
            locToOffset srcStrRange.start srcString

        srcEnd =
            locToOffset srcStrRange.end srcString

        astStart =
            locToOffset astStrRange.start astString - 40

        astEnd =
            locToOffset astStrRange.end astString - 40

        dbg =
            Debug.log "astString length" (String.length astString)
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


order : SrcToAstMapping -> SrcToAstMapping -> Order
order a b =
    let
        aRange =
            a.srcStringOffsetEnd - a.srcStringOffsetStart

        bRange =
            b.srcStringOffsetEnd - b.srcStringOffsetStart

        diff =
            aRange - bRange
    in
    if diff < 0 then
        LT

    else if diff == 0 then
        EQ

    else
        GT
