module CardContent exposing (render, CardContent, Msg(..), encode, decode, update)

import Html exposing (Html, div, text, button, textarea)
import Html.Attributes exposing (class, value, placeholder, style, disabled)
import Html.Events as HE

import Markdown.Parser as Markdown
import Markdown.Renderer
import Markdown.Html

import Json.Encode as JE
import Json.Decode as JD

type alias CardContent =
    { text: String
    , done: Bool
    }

type Msg
    = SetDone Bool

render : CardContent -> Html Msg
render content = div
    [ class "card-content" ]
    ( [ case content.text
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        of
            Ok rendered ->
                div [ class "markdown" ] rendered

            Err errors ->
                div [ class "markdown-errors" ] [ text errors ]

    ] |> detectMaths content.text )

update : Msg -> CardContent -> (CardContent, Bool)
update msg content = case msg of
    SetDone done -> ({ content | done = done }, True)

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"

renderer : Markdown.Renderer.Renderer (Html Msg)
renderer = let default = Markdown.Renderer.defaultHtmlRenderer in
    { default
    | html = Markdown.Html.oneOf
        [
        ]
    }

detectMaths : String -> List (Html Msg) -> List (Html Msg)
detectMaths content tags = case hasMaths content of
    True  -> [ renderMaths tags ]
    False -> tags

hasMaths : String -> Bool
hasMaths s = (String.contains "$" s) || (String.contains "\\(" s) || (String.contains "\\[" s)

renderMaths : List (Html Msg) -> Html Msg
renderMaths children = Html.node "may-contain-maths" [ class "maths" ] children

decode : JD.Decoder CardContent
decode = JD.map2 CardContent
    (JD.field "text" JD.string)
    (JD.field "done" JD.bool)

encode : CardContent -> JD.Value
encode content = JE.object
    [ ("text",     JE.string content.text)
    , ("done",     JE.bool   content.done)
    ]
