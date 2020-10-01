module CardContent exposing (render, CardContent, Msg(..))

import Html exposing (Html, div, text, button, textarea)
import Html.Attributes exposing (class, value, placeholder, style, disabled)
import Html.Events as HE

import Markdown.Parser as Markdown
import Markdown.Renderer
import Markdown.Html

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
            |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
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

renderer : Markdown.Renderer.Renderer (Html Msg)
renderer = let default = Markdown.Renderer.defaultHtmlRenderer in
    { default
    | html = Markdown.Html.oneOf
        [ Markdown.Html.tag "maths" renderMaths
        , Markdown.Html.tag "math"  renderMaths
        ]
    }

renderMaths : List (Html Msg) -> Html Msg
renderMaths children = div [ class "maths" ] children
