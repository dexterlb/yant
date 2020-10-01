module CardContent exposing (render, CardContent, Msg(..))

import Html exposing (Html, div, text, button, textarea)
import Html.Attributes exposing (class, value, placeholder, style, disabled)
import Html.Events as HE

import Markdown.Parser as Markdown
import Markdown.Renderer

type alias CardContent =
    { text: String
    }

type Msg
    = Foo

render : CardContent -> Html Msg
render content = div
    [ class "card-content" ]
    [ case content.text
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
        of
            Ok rendered ->
                div [ class "markdown" ] rendered

            Err errors ->
                text errors
    ]

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
