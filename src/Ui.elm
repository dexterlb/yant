module Ui exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Dict as Dict
import List.Nonempty as NE

import Cards exposing (..)
import PathTree as PT exposing (PathTree)


type alias Model =
    { cards: Cards
    , rootCard: CardID
    , context: Context
    }

type alias Context =
    { edit: Maybe EditContext
    , expanded: PathTree ()
    }

type alias EditContext =
    { path: CardPath
    }

type Msg =
    Foo

init : CardID -> Model
init rootCard =
    { cards = noCards
    , rootCard = rootCard
    , context =
        { edit = Nothing
        , expanded = PT.empty
        }
    }

update : (Msg -> m) -> Msg -> Model -> ( Model, Cmd m )
update liftMsg msg model = case msg of
    Foo -> (model, Cmd.none)


viewCard : Context -> CardPath -> Cards -> Html Msg
viewCard ctx path cards = case Dict.get (NE.head path) cards of
    Nothing   -> div [ class "card", class "waiting" ] [ text "waiting for card" ]
    Just card -> case isExpanded ctx path of
        True ->
            div [ class "card", class "expanded" ]
                [ viewCardBody card (isBeingEdited ctx path)
                , viewCardChildren ctx path cards card.children
                ]
        False ->
            div [ class "card", class "collapsed" ]
                [ viewCardBody card (isBeingEdited ctx path)
                ]

viewCardChildren : Context -> CardPath -> Cards -> List CardID -> Html Msg
viewCardChildren ctx path cards childIDs =
    div [ class "card-children" ]
        (List.map (\id -> viewCard ctx (NE.cons id path) cards) childIDs)

viewCardBody : Card -> Maybe EditContext -> Html Msg
viewCardBody card mectx = div [ class "card-body" ]
    [ text card.text ]


view : Model -> Html Msg
view { context, cards, rootCard } =
    div []
        [ viewCard context (NE.fromElement rootCard) cards
        ]

-- Helpers

isBeingEdited : Context -> CardPath -> Maybe EditContext
isBeingEdited ctx path = ctx.edit |> Maybe.andThen (\ectx ->
    case path == ectx.path of
        True  -> Just ectx
        False -> Nothing)

isExpanded : Context -> CardPath -> Bool
isExpanded ctx path = PT.member path ctx.expanded
