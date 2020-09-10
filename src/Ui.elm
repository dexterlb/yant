module Ui exposing (Model, Msg, init, update, view, InputMsg(..), Action(..), Actions, pushMsg)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Element
import Element.Input

import Dict as Dict
import List.Nonempty as NE

import Cards as Cards exposing (Cards, Card, CardID, CardPath, noCards)
import PathTree as PT exposing (PathTree)


type alias Model =
    { cards: Cards
    , rootCard: CardID
    , context: Context
    }

type alias Context =
    { state: UserState
    , expanded: PathTree ()
    }

type alias EditContext =
    { path: CardPath
    , text: String
    }

type UserState
    = None
    | Selected CardPath
    | Editing EditContext

type Msg
    = TextChanged String
    | SelectCard  CardPath
    | EditCard    CardPath
    | SaveEdit

type InputMsg
    = GotCard Card

type Action
    = GetCard CardID
    | SaveCard Card

type alias Actions = List Action


init : CardID -> (Model, Cmd m, Actions)
init rootCard =
    (
        { cards = noCards
        , rootCard = rootCard
        , context =
            { state = None
            , expanded = PT.empty
            }
        }
    ,   Cmd.none
    ,   [GetCard "root"]
    )

update : Msg -> Model -> ( Model, Cmd Msg, Actions )
update msg model = let oldContext = model.context in case msg of
    TextChanged text -> ( updateEditContext (\ectx -> { ectx | text = text }) model, Cmd.none, [])
    SelectCard path  -> ( newState (Selected path) model, Cmd.none, [] )
    EditCard path    -> case Dict.get (NE.head path) model.cards of
        Nothing -> (model, Cmd.none, [])
        Just card -> ( newState (Editing { path = path, text = card.text }) model, Cmd.none, [])
    SaveEdit -> case model.context.state of
        Editing ectx -> let (cards, cmd, actions) = editCard ectx model.cards in
            ( { model | cards = cards, context = { oldContext | state = None } }, cmd, actions )
        _ -> (model, Cmd.none, [])

pushMsg : InputMsg -> Model -> ( Model, Cmd Msg, Actions )
pushMsg inMsg model = case inMsg of
    GotCard card ->
        ( { model | cards = Cards.add card model.cards }, Cmd.none, [] )


viewCard : Context -> CardPath -> Cards -> Html Msg
viewCard ctx path cards = case Dict.get (NE.head path) cards of
    Nothing   -> div [ class "card", class "waiting" ] [ text "waiting for card" ]
    Just card -> case isExpanded ctx path of
        True ->
            div [ class "card", class "expanded" ]
                [ viewCardBody path card ctx.state
                , viewCardChildren ctx path cards card.children
                ]
        False ->
            div [ class "card", class "collapsed" ]
                [ viewCardBody path card ctx.state
                ]

viewCardChildren : Context -> CardPath -> Cards -> List CardID -> Html Msg
viewCardChildren ctx path cards childIDs =
    div [ class "card-children" ]
        (List.map (\id -> viewCard ctx (NE.cons id path) cards) childIDs)

viewCardBody : CardPath -> Card -> UserState -> Html Msg
viewCardBody path card state = case state of
    None -> viewPlainCardBody path card
    Editing ectx -> case ectx.path == path of
        True -> viewEditingCardBody path card ectx
        False -> viewPlainCardBody path card
    Selected spath -> case spath == path of
        True -> viewSelectedCardBody path card
        False -> viewPlainCardBody path card



viewEditingCardBody : CardPath -> Card -> EditContext -> Html Msg
viewEditingCardBody path card ectx = div [ class "card-body", class "editing" ]
    [ Element.layout [] <| Element.Input.multiline []
        { text = ectx.text
        , spellcheck = False
        , placeholder = Just (Element.Input.placeholder [] (Element.text "enter some note text"))
        , label = Element.Input.labelHidden "note text"
        , onChange = TextChanged
        }
    , Element.layout [] <| Element.Input.button []
        { onPress = Just SaveEdit
        , label = Element.text "save"
        }
    ]

viewPlainCardBody : CardPath -> Card -> Html Msg
viewPlainCardBody path card = div
    [ class "card-body", class "plain"
    , onClick (SelectCard path)
    ]
    [ text (case card.text of
        "" -> "<empty>"
        text -> text ) ]

viewSelectedCardBody : CardPath -> Card -> Html Msg
viewSelectedCardBody path card = div [ class "card-body", class "selected" ]
    [ text card.text
    , viewCardButtonBar path card]

viewCardButtonBar : CardPath -> Card -> Html Msg
viewCardButtonBar path card = div [ class "button-bar" ]
    [ Element.layout [] <| Element.Input.button []
        { onPress = Just (EditCard path)
        , label = Element.text "edit"
        }
    ]


view : Model -> Html Msg
view { context, cards, rootCard } =
    div []
        [ viewCard context (NE.fromElement rootCard) cards
        ]

-- Helpers

isExpanded : Context -> CardPath -> Bool
isExpanded ctx path = PT.member path ctx.expanded

setCardText : Cards -> CardID -> String -> Cards
setCardText cards id text = Cards.update id (\card -> { card | text = text }) cards

-- todo: rewrite this to use updateState
updateEditContext : (EditContext -> EditContext) -> Model -> Model
updateEditContext f model = let oldContext = model.context in case model.context.state of
    Editing ectx -> { model | context = { oldContext | state = Editing (f ectx) } }
    _            -> model

newState : UserState -> Model -> Model
newState state = updateState (\_ -> state)

updateState : (UserState -> UserState) -> Model -> Model
updateState f model = let oldContext = model.context in
    { model | context = { oldContext | state = f oldContext.state } }


editCard : EditContext -> Cards -> (Cards, Cmd Msg, Actions)
editCard ectx cards = case Dict.get (NE.head ectx.path) cards of
    Nothing -> (cards, Cmd.none, [])
    Just oldCard -> let card = { oldCard | text = ectx.text } in
        (Cards.add card cards, Cmd.none, [SaveCard card])
