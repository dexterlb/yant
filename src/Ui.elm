module Ui exposing (Model, Msg, init, update, view)

import Html exposing (Html)

import Cards exposing (..)


type alias Model =
    { cards: Cards
    }


type Msg =
    Foo

init : Model
init =
    { cards = noCards
    }

update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update liftMsg msg model = case msg of
    Foo -> (model, Cmd.none)


view : Model -> Html Msg
view { } =
    Html.div [] [Html.text "foo"]
