module Demo exposing (main)

import Browser
import Elm.DSLParser
import Elm.Parser
import Elm.Pretty
import Elm.Processing
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as JD
import SrcToAstMapper exposing (SrcToAstMapping, mapSrcToAst)



--
-- MAIN
--


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { src = ""
    , cursorPos = 0
    , astStyle = Pretty
    , parseResult = ""
    , stringifiedAst = ""
    , srcToAstMap = []
    , activeMapping = Nothing
    }
        |> update (SrcChanged """module Foo exposing (foo)

-- some comment

{-| Docs -}
foo = 1
""")



--
-- TYPES
--


type Msg
    = SrcChanged String
    | CursorMoved Int
    | ToggleAstStyle


type AstStyle
    = Condensed
    | Pretty


type alias Model =
    { src : String
    , cursorPos : Int
    , astStyle : AstStyle
    , parseResult : String
    , stringifiedAst : String
    , srcToAstMap : List SrcToAstMapping
    , activeMapping : Maybe SrcToAstMapping
    }



--
-- UPDATE
--


update : Msg -> Model -> Model
update msg model =
    case msg of
        SrcChanged newSrc ->
            let
                parseResult : String
                parseResult =
                    let
                        res =
                            Elm.Parser.parse newSrc
                                |> Result.map (Elm.Processing.process Elm.Processing.init)
                    in
                    case res of
                        Ok value ->
                            Debug.toString value

                        Err errs ->
                            Debug.toString errs

                stringifiedAst : String
                stringifiedAst =
                    getStyledAst parseResult model.astStyle

                srcToAstMap : List SrcToAstMapping
                srcToAstMap =
                    mapSrcToAst newSrc stringifiedAst
            in
            { model
                | src = newSrc
                , parseResult = parseResult
                , stringifiedAst = stringifiedAst
                , srcToAstMap = srcToAstMap
            }

        CursorMoved newPos ->
            let
                -- NOTE: need to add one to the cusorPos as a text area starts at 0 but the AST starts at 1
                newCursorPos =
                    newPos + 1
            in
            { model
                | activeMapping = getActiveMapping model.srcToAstMap newCursorPos
                , cursorPos = newCursorPos
            }

        ToggleAstStyle ->
            let
                astStyle : AstStyle
                astStyle =
                    case model.astStyle of
                        Condensed ->
                            Pretty

                        Pretty ->
                            Condensed

                stringifiedAst : String
                stringifiedAst =
                    getStyledAst model.parseResult astStyle

                srcToAstMap : List SrcToAstMapping
                srcToAstMap =
                    mapSrcToAst model.src stringifiedAst

                activeMapping : Maybe SrcToAstMapping
                activeMapping =
                    getActiveMapping srcToAstMap model.cursorPos
            in
            { model
                | astStyle = astStyle
                , stringifiedAst = stringifiedAst
                , srcToAstMap = srcToAstMap
                , activeMapping = activeMapping
            }


getActiveMapping : List SrcToAstMapping -> Int -> Maybe SrcToAstMapping
getActiveMapping srcToAstMap cursorPos =
    let
        matches : List SrcToAstMapping
        matches =
            srcToAstMap
                |> List.filter
                    (\m -> cursorPos >= m.srcStringOffsetStart && cursorPos <= m.srcStringOffsetEnd)

        smallestRangeMatch : Maybe SrcToAstMapping
        smallestRangeMatch =
            matches
                |> List.head

        largestRangeMatch : Maybe SrcToAstMapping
        largestRangeMatch =
            matches
                |> List.reverse
                |> List.head
    in
    case ( smallestRangeMatch, largestRangeMatch ) of
        ( Just srm, Just lrm ) ->
            if srm.srcStringOffsetStart == lrm.srcStringOffsetStart && srm.srcStringOffsetStart == cursorPos then
                largestRangeMatch

            else
                smallestRangeMatch

        _ ->
            smallestRangeMatch


getStyledAst : String -> AstStyle -> String
getStyledAst condensedAst astStyle =
    case astStyle of
        Condensed ->
            condensedAst

        Pretty ->
            prettifyAst condensedAst



--
-- VIEW
--


fontFamily =
    "Courier New"


fontSize =
    "14px"


view : Model -> Html Msg
view m =
    Html.div []
        [ Html.div
            [ Html.Attributes.style "height" "40px" ]
            [ Html.button
                [ Html.Attributes.style "margin-left" "10px"
                , Html.Attributes.style "margin-top" "10px"
                , Html.Attributes.style "width" "120px"
                , Html.Events.onClick ToggleAstStyle
                ]
                [ Html.text
                    (case m.astStyle of
                        Condensed ->
                            "Pretty AST"

                        Pretty ->
                            "Condensed AST"
                    )
                ]
            ]
        , Html.div
            [ Html.Attributes.style "display" "flex" ]
            [ Html.textarea
                [ Html.Events.onInput SrcChanged
                , onMouseUp CursorMoved
                , onKeyUp CursorMoved
                , Html.Attributes.value m.src
                , Html.Attributes.cols 50
                , Html.Attributes.rows 20
                , Html.Attributes.style "font-family" fontFamily
                , Html.Attributes.style "font-size" fontSize
                , Html.Attributes.style "margin-left" "10px"
                ]
                []
            , astView m.stringifiedAst m.activeMapping m.astStyle
            ]
        ]


astView : String -> Maybe SrcToAstMapping -> AstStyle -> Html Msg
astView astString activeMapping astStyle =
    let
        textWrap =
            case astStyle of
                Pretty ->
                    "nowrap"

                Condensed ->
                    "wrap"

        normalStyle =
            [ Html.Attributes.style "white-space" "pre"
            , Html.Attributes.style "font-family" fontFamily
            , Html.Attributes.style "font-size" fontSize
            , Html.Attributes.style "text-wrap" textWrap
            ]

        hlStyle =
            [ Html.Attributes.style "background-color" "#FFFF00"
            , Html.Attributes.style "white-space" "pre"
            , Html.Attributes.style "font-family" fontFamily
            , Html.Attributes.style "font-size" fontSize
            , Html.Attributes.style "text-wrap" textWrap
            ]

        content =
            case activeMapping of
                Nothing ->
                    [ Html.span normalStyle [ Html.text astString ] ]

                Just m ->
                    let
                        hlStart =
                            m.astStringOffsetStart

                        hlEnd =
                            m.astStringOffsetEnd

                        beforeStr =
                            String.slice 0 hlStart astString

                        hlStr =
                            String.slice hlStart hlEnd astString

                        afterStr =
                            String.slice hlEnd (String.length astString) astString
                    in
                    [ Html.span normalStyle [ Html.text beforeStr ]
                    , Html.span hlStyle [ Html.text hlStr ]
                    , Html.span normalStyle [ Html.text afterStr ]
                    ]
    in
    Html.div [ Html.Attributes.style "margin-left" "20px" ] content


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (JD.map tagger (JD.at [ "target", "selectionStart" ] JD.int))


onMouseUp : (Int -> msg) -> Html.Attribute msg
onMouseUp tagger =
    Html.Events.on "mouseup" (JD.map tagger (JD.at [ "target", "selectionStart" ] JD.int))


{-| Prettify the AST string using elm-syntax-dsl.

elm-syntax-dsl will only parse a full Elm module therefore a dummy module
definition is added in front as well as assigning the AST a name before passing
it to elm-syntax-dsl. After getting formatted the approriate cleanup is required
to remove the added dummy module definition and AST name which leaves the
original AST string now formatted.

-}
prettifyAst : String -> String
prettifyAst elmCode =
    let
        formattedLines =
            elmCode
                |> (++) "module DisplayAst exposing (ast)\nast = "
                |> Elm.DSLParser.parse
                |> Result.map (Elm.Pretty.pretty 120)
                |> Result.withDefault ""
                |> String.lines

        nLines =
            List.length formattedLines
    in
    formattedLines
        |> List.reverse
        |> List.take (nLines - 4)
        |> List.reverse
        |> List.map (\l -> String.reverse l |> String.slice 0 -4 |> String.reverse)
        |> String.join "\n"
