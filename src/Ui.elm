module Ui exposing (Model, Msg, init, update, view, InputMsg(..), Action(..), Actions, pushMsg)

import Html exposing (Html, div, text, button, textarea)
import Html.Attributes exposing (class, value, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Color

import Markdown.Option
import Markdown.Render

import Random
import Random.String
import Random.Char

import Dict as Dict
import List.Nonempty as NE
import Set exposing (Set)

import Cards as Cards exposing (Cards, Card, CardID, CardPath, noCards)
import PathTree as PT exposing (PathTree)
import Utils


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

type CardState
    = CardNone
    | CardSelected
    | CardEditing

type Msg
    = TextChanged String
    | SelectCard  CardPath
    | EditCard    CardPath
    | AddChild    CardPath
    | Expand      CardPath
    | Collapse    CardPath
    | AddChildWithID CardPath CardID
    | SaveEdit
    | MarkdownMsg Markdown.Render.MarkdownMsg

type InputMsg
    = GotCard Card

type Action
    = GetCard CardID
    | SaveCard Card

type alias Actions = List Action


init : CardID -> (Model, Cmd m, Actions)
init rootCard =
    let
        model =
            { cards = noCards
            , rootCard = rootCard
            , context =
                { state = None
                , expanded = PT.empty
                }
            }
    in let
        (model1, actions) = syncCards model
    in
        (model1, Cmd.none, actions)

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
    AddChild path -> ( model, randomID (AddChildWithID path), [])
    AddChildWithID path id ->
        let model1 = { model | cards = addChildToCard id (NE.head path) model.cards }
        in let (model2, actions) = expand path model1
        in
            ( model2, Cmd.none, (saveCard (NE.head path) model2.cards) ++ actions )
    Expand path ->
        let (model1, actions) = expand path model
        in
            ( model1, Cmd.none, actions )
    Collapse path ->
        let (model1, actions) = collapse path model
        in
            ( model1, Cmd.none, actions )
    MarkdownMsg _ -> ( model, Cmd.none, [] )


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
                [ viewCardBody ctx path card
                , viewCardChildren ctx path cards card.children
                ]
        False ->
            div [ class "card", class "collapsed" ]
                [ viewCardBody ctx path card
                ]

viewCardChildren : Context -> CardPath -> Cards -> List CardID -> Html Msg
viewCardChildren ctx path cards childIDs =
    div [ class "card-children" ]
        (List.map (\id -> viewCard ctx (NE.cons id path) cards) childIDs)

viewCardBody : Context -> CardPath -> Card -> Html Msg
viewCardBody ctx path card =
    case ctx.state of
        None -> viewPlainCardBody ctx path card
        Editing ectx -> case ectx.path == path of
            True -> viewEditingCardBody ctx path card ectx
            False -> viewPlainCardBody ctx path card
        Selected spath -> case spath == path of
            True -> viewSelectedCardBody ctx path card
            False -> viewPlainCardBody ctx path card



viewEditingCardBody : Context -> CardPath -> Card -> EditContext -> Html Msg
viewEditingCardBody ctx path card ectx = div
    [ class "card-body", class "editing", cardColour path CardEditing ]
    [ viewCardControls ctx path
    , textarea
        [ value ectx.text
        , placeholder "enter some note text"
        , onInput TextChanged
        ] []
    , button
        [ onClick SaveEdit ]
        [ text "save" ]
    ]

viewPlainCardBody : Context -> CardPath -> Card -> Html Msg
viewPlainCardBody ctx path card = div
    [ class "card-body", class "plain"
    , onClick (SelectCard path)
    , cardColour path CardNone
    ]
    [ viewCardControls ctx path
    , viewCardContent card ]

viewSelectedCardBody : Context -> CardPath -> Card -> Html Msg
viewSelectedCardBody ctx path card = div
    [ class "card-body", class "selected", cardColour path CardSelected ]

    [ viewCardControls ctx path
    , viewCardContent card
    , viewCardButtonBar path card]

viewCardContent : Card -> Html Msg
viewCardContent card = div
    [ class "card-content" ]
    [ Markdown.Render.toHtml Markdown.Option.ExtendedMath (case card.text of
        "" -> "<empty>"
        text -> text ) |> Html.map MarkdownMsg ]


viewCardButtonBar : CardPath -> Card -> Html Msg
viewCardButtonBar path card = div [ class "button-bar" ]
    [ button
        [ onClick (EditCard path) ]
        [ text "edit" ]
    , button
        [ onClick (AddChild path) ]
        [ text "add child" ]
    ]

viewCardControls : Context -> CardPath -> Html Msg
viewCardControls ctx path = case isExpanded ctx path of
    True  -> viewExpandedCardControls  ctx path
    False -> viewCollapsedCardControls ctx path

viewExpandedCardControls : Context -> CardPath -> Html Msg
viewExpandedCardControls ctx path = div [ class "controls" ]
    [ button
        [ onClick (Collapse path) ]
        [ div [ class "sr-only" ] [ text "collapse" ]
        , div [ class "icon" ]    [ text "▼" ]
        ]
    ]

viewCollapsedCardControls : Context -> CardPath -> Html Msg
viewCollapsedCardControls ctx path = div [ class "controls" ]
    [ button
        [ onClick (Expand path) ]
        [ div [ class "sr-only" ] [ text "expand" ]
        , div [ class "icon" ]    [ text "▶" ]
        ]
    ]


view : Model -> Html Msg
view { context, cards, rootCard } =
    div []
        [ viewCard context (NE.fromElement rootCard) cards
        ]

-- Helpers

isExpanded : Context -> CardPath -> Bool
isExpanded ctx path = PT.member path ctx.expanded

expand : CardPath -> Model -> (Model, Actions)
expand path model =
    let oldContext = model.context
    in let
        model1 = { model | context =
            { oldContext | expanded = PT.put path () oldContext.expanded } }
    in syncCards model1

collapse : CardPath -> Model -> (Model, Actions)
collapse path model =
    let oldContext = model.context
    in let
        model1 = { model | context =
            { oldContext | expanded = PT.drop path oldContext.expanded } }
    in syncCards model1

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

saveCard : CardID -> Cards -> Actions
saveCard id cards = case Dict.get id cards of
    Nothing -> []
    Just card -> [SaveCard card]

addChildToCard : CardID -> CardID -> Cards -> Cards
addChildToCard childID parentID cards =
    case Dict.get parentID cards of
        Nothing -> cards
        Just oldParent -> let parent = { oldParent | children = childID :: oldParent.children } in
            Cards.add parent cards

syncCards : Model -> (Model, Actions)
syncCards m = (m, List.map GetCard (Set.toList <| missingCards m))

missingCards : Model -> Set CardID
missingCards m = Set.diff (neededCards m) (Set.fromList <| Dict.keys m.cards)

neededCards : Model -> Set CardID
neededCards m = neededCardsFrom (NE.fromElement m.rootCard) m

-- todo: tail optimisation
neededCardsFrom : CardPath -> Model -> Set CardID
neededCardsFrom path m =
    Set.insert (NE.head path) <|
        case isExpanded m.context path of
            True ->
                (List.foldr
                    (\child all -> Set.union all (neededCardsFrom (NE.cons child path) m)) Set.empty
                    (childrenOf m.cards (NE.head path)))
            False -> Set.empty

childrenOf : Cards -> CardID -> List CardID
childrenOf cards id = case Dict.get id cards of
    Nothing -> []
    Just card -> card.children
-- Utils

randomID : (String -> Msg) -> Cmd Msg
randomID f = Random.generate f
    (Random.String.string 32 Random.Char.english)

cardColour : CardPath -> CardState -> Html.Attribute Msg
cardColour path state =
    let
        hue = interpolate (25 / 360.0) (200 / 360.0) (Utils.hash01 (NE.head path))
        sat = case state of
            CardNone      -> 0.2
            CardSelected  -> 0.5
            CardEditing   -> 0
    in let
        colour = Color.hsl hue sat 0.8 |> Color.toCssString
    in
        style "background-color" colour

interpolate : Float -> Float -> Float -> Float
interpolate a b x = x * (b - a) + a
