module Ui exposing (Model, Msg, init, update, view, InputMsg(..), Action(..), Actions, pushMsg)

import Html exposing (Html, div, text, button, textarea, input)
import Html.Attributes exposing (class, value, placeholder, style, disabled, type_, checked)
import Html.Events as HE
import Color

import Random
import Random.String
import Random.Char

import Dict as Dict exposing (Dict)
import List.Nonempty as NE
import Set exposing (Set)
import Json.Decode as JD

import Cards as Cards exposing (Cards, Card, CardID, CardPath, noCards)
import PathTree as PT exposing (PathTree)
import Utils as Utils exposing (onClick)
import CardContent as CardContent
import Calendar
import Settings as Settings exposing (Settings)


type alias Model =
    { cards: Cards
    , rootCard: CardID
    , state: UserState
    , expanded: PathTree ()
    , error: Maybe ErrorMessage
    , settings: Settings
    , selectedCard : Maybe (CardContent.Model, CardPath)
    , clipboard : Maybe Clipboard
    }

type alias Clipboard = (CardPath, ClipboardState)

type ErrorMessage
    = NotImplemented
    | UnlinkWithoutParent

type ClipboardState
    = Move
    | Link
    | Copy

type InsertMode
    = FirstChild
    | LastChild
    | Before
    | After

type alias Insertion = (InsertMode, CardPath)

type UserState
    = None
    | PendingEdit CardPath

type CardState
    = CardNone
    | CardSelected
    | CardEditing

type Msg
    = SelectCard      CardPath
    | DeselectCard
    | UnlinkCard      CardPath
    | AddChild        Insertion
    | Expand          CardPath
    | Collapse        CardPath
    | AddChildWithID  Insertion CardID
    | ContentMsg      CardContent.Msg
    | NotImplementedMsg
    | SetClipboard    (Maybe Clipboard)
    | PasteChild      Insertion Clipboard
    | ClearError

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
            , state = None
            , expanded = PT.empty
            , error = Nothing
            , settings = Settings.default
            , clipboard = Nothing
            , selectedCard = Nothing
            }
    in let
        (model1, actions) = syncCards model
    in
        (model1, Cmd.none, actions)

update : Msg -> Model -> ( Model, Cmd Msg, Actions )
update msg model = case msg of
    SelectCard path  ->
        let (model1, actions1) = selectedCardEvent CardContent.Destroy model
        in let model2 = { model1 | selectedCard = Just (CardContent.emptyModel (fetch (NE.head path) model.cards).content, path) }
        in
            ( model2, Cmd.none, actions1 )
    DeselectCard     ->
        let (model1, actions1) = selectedCardEvent CardContent.Destroy model
        in let model2 = { model1 | selectedCard = Nothing }
        in
            ( model2, Cmd.none, actions1 )
    
    UnlinkCard path -> case NE.tail path |> List.head of
        Nothing ->
            ( setError UnlinkWithoutParent model, Cmd.none, [] )
        Just parent ->
            let model1 = { model | cards = delChildFromCard parent (NE.head path) model.cards }
            in
                ( model1, Cmd.none, (saveCard parent model1.cards) )
    AddChild ins -> ( model, randomID (AddChildWithID ins), [])
    AddChildWithID ins id ->
        let (model2, newPath, actions2) = insertChildWithID ins id model
        in
            ( newState (PendingEdit newPath) model2, Cmd.none, actions2 )

    PasteChild ins (oldPath, Move) ->
        let
            (model1, cmd1, actions1) = update (UnlinkCard oldPath) model
        in let
            (model2, _, actions2) = insertChildWithID ins (NE.head oldPath) model1
        in  (model2, cmd1, actions1 ++ actions2)

    PasteChild ins (oldPath, Copy) ->
       ( setError NotImplemented model, Cmd.none, [] )

    PasteChild ins (oldPath, Link) ->
        let (model2, _, actions2) = insertChildWithID ins (NE.head oldPath) model
        in (model2, Cmd.none, actions2)

    Expand path ->
        let (model1, actions) = expand path model
        in
            ( model1, Cmd.none, actions )
    Collapse path ->
        let (model1, actions) = collapse path model
        in
            ( model1, Cmd.none, actions )

    SetClipboard clip ->
        ( setClipboard clip model, Cmd.none, [] )

    ContentMsg contentMsg ->
        case model.selectedCard of
            Nothing -> ( model, Cmd.none, [] )
            Just (content, path) ->
                CardContent.update contentMsg content
                    |> processContentResult model path
                    |> emptyCmd

    NotImplementedMsg ->
       ( setError NotImplemented model, Cmd.none, [] )

    ClearError ->
       ( clearError model, Cmd.none, [] )

insertChildWithID : Insertion -> CardID -> Model -> (Model, CardPath, Actions)
insertChildWithID ins id model =
    let (newCards, newPath) = insertChild id ins model.cards
    in let
        model1 = { model | cards = newCards }
        parentPath = getParentPath newPath
    in let (model2, actions1) = expand parentPath model1
    in
        ( setClipboard Nothing model2
        , newPath
        , (saveCard (NE.head parentPath) model2.cards) ++ actions1 )

emptyCmd : (a, b) -> (a, Cmd msg, b)
emptyCmd (x, y) = (x, Cmd.none, y)

setClipboard : Maybe Clipboard -> Model -> Model
setClipboard clip model = { model | clipboard = clip }

selectedCardEvent : CardContent.Event -> Model -> (Model, Actions)
selectedCardEvent evt model = case model.selectedCard of
    Nothing -> (model, [])
    Just (contentModel, path) ->
        CardContent.event evt contentModel
            |> processContentResult model path

processContentResult : Model -> CardPath -> (CardContent.Model, CardContent.Actions) -> (Model, Actions)
processContentResult model path (content, cactions) =
    let
        card = fetch (NE.head path) model.cards
        model1 = { model | selectedCard = Just (content, path) }
    in let
        (model2, actions1) = case card.content == content.data of
            True -> (model1, [])
            False -> let model11 = { model1 | cards = Cards.add { card | content = content.data } model.cards }
                     in (model11, saveCard (NE.head path) model11.cards)
    in let
        (model3, actions3) = processContentActions cactions model2
    in (model3, actions1 ++ actions3)


processContentActions : CardContent.Actions -> Model -> (Model, Actions)
processContentActions cas model = case cas of
    [] -> (model, [])
    (c :: cs) -> let (model1, actions1) = processContentAction c model
                 in let (model2, actions2) = processContentActions cs model
                 in (model2, actions1 ++ actions2)

processContentAction : CardContent.Action -> Model -> (Model, Actions)
processContentAction ca model = case ca of
    CardContent.Dunno -> Debug.todo "implement me"

pushMsg : InputMsg -> Model -> ( Model, Cmd Msg, Actions )
pushMsg inMsg model = case inMsg of
    GotCard card ->
        let model1 = { model | cards = Cards.add card model.cards }
        in
            case model1.state of
                PendingEdit path -> case (NE.head path) == card.id of
                    True -> let ( model3, cmd, actions3) = update (SelectCard path) model1
                            in let
                                ( model4, actions4 ) = selectedCardEvent CardContent.BeginEdit model3
                            in (model4, cmd, actions4)
                    False -> (model1, Cmd.none, [])
                _ -> (model1, Cmd.none, [])


viewCard : Model -> CardPath -> Cards -> Html Msg
viewCard model path cards = case Dict.get (NE.head path) cards of
    Nothing   -> div [ class "card", class "waiting" ] [ text "waiting for card" ]
    Just card -> case isExpanded model path of
        True ->
            div [ class "card", class "expanded" ]
                [ viewCardBody model path card
                , viewCardChildren model path cards card.children
                ]
        False ->
            div [ class "card", class "collapsed" ]
                [ viewCardBody model path card
                ]

viewCardChildren : Model -> CardPath -> Cards -> List CardID -> Html Msg
viewCardChildren model path cards childIDs =
    div [ class "card-children" ]
        (List.map (\id -> viewCard model (NE.cons id path) cards) childIDs)

viewCardBody : Model -> CardPath -> Card -> Html Msg
viewCardBody model path card =
    case (model.state, model.selectedCard) of
        (_, Nothing) -> viewPlainCardBody model path card
        (_, Just (content, selPath)) ->
            case path == selPath of
                True  -> viewSelectedCardBody model path card content
                False -> viewPlainCardBody model path card

viewPlainCardBody : Model -> CardPath -> Card -> Html Msg
viewPlainCardBody model path card = div
    [ class "card-body", class "plain"
    , onClick (SelectCard path)
    , cardColour path CardNone
    ]
    [ viewCardControls model path card
    , div [ class "card-vbox" ]
        [ viewCardContentDataOnly card.content
        ]
    ]

viewSelectedCardBody : Model -> CardPath -> Card -> CardContent.Model -> Html Msg
viewSelectedCardBody model path card cardContent = div
    [ class "card-body", class "selected", cardColour path CardSelected ]

    [ viewCardControls model path card
    , div [ class "card-vbox" ]
        [ viewCardButtonBar path cardContent model.clipboard
        , viewCardContent card.id cardContent
        ]
    ]

viewCardContentDataOnly : CardContent.Data -> Html Msg
viewCardContentDataOnly cardContent
    =  CardContent.viewDataOnly cardContent
    |> Html.map ContentMsg

viewCardContent : CardID -> CardContent.Model -> Html Msg
viewCardContent id cardContent
    =  CardContent.view cardContent
    |> Html.map ContentMsg


viewCardButtonBar : CardPath -> CardContent.Model -> Maybe Clipboard -> Html Msg
viewCardButtonBar path cardContent clip = case clip of
    Nothing    -> viewCardToolbar path cardContent
    Just clips -> viewCardPasteBar path cardContent clips

viewCardPasteBar : CardPath -> CardContent.Model -> (CardPath, ClipboardState) -> Html Msg
viewCardPasteBar path cardContent clip = div [ class "button-bar" ] (
    [ button
        [ onClick DeselectCard ]
        [ text "deselect" ]
    , button
        [ onClick (SetClipboard Nothing) ]
        [ text "cancel" ]
    , button
        [ onClick (PasteChild (FirstChild, path) clip) ]
        [ text "paste as child" ]
    ] ++ case NE.tail path |> List.isEmpty of
        True  -> []
        False ->
            [ button
                [ onClick (PasteChild (Before, path) clip) ]
                [ text "paste before" ]
            , button
                [ onClick (PasteChild (After, path) clip) ]
                [ text "paste after" ]
            ])

viewCardToolbar : CardPath -> CardContent.Model -> Html Msg
viewCardToolbar path cardContent = div [ class "button-bar" ]
    [ Html.map ContentMsg (CardContent.buttons cardContent)
    , div [ class "button-group", class "button-group-general" ] (
        [ button
            [ onClick DeselectCard ]
            [ text "deselect" ]
        , button
            [ onClick (SetClipboard (Just (path, Move))) ]
            [ text "move" ]
        , button
            [ onClick (SetClipboard (Just (path, Copy))) ]
            [ text "copy" ]
        , button
            [ onClick (SetClipboard (Just (path, Link))) ]
            [ text "link" ]
        , button
            [ onClick (AddChild (FirstChild, path)) ]
            [ text "add child" ]
        ] ++ (case NE.tail path |> List.isEmpty of
            True  -> []
            False ->
                [ button
                    [ onClick (UnlinkCard path) ]
                    [ text "unlink" ]
                , button
                    [ onClick (AddChild (Before, path)) ]
                    [ text "add before" ]
                , button
                    [ onClick (AddChild (After, path)) ]
                    [ text "add after" ]
                ])
        )
    ]

viewCardControls : Model -> CardPath -> Card -> Html Msg
viewCardControls model path card =
    case List.isEmpty card.children of
        True  -> viewChildlessCardControls  model path
        False ->
            case isExpanded model path of
                True  -> viewExpandedCardControls  model path
                False -> viewCollapsedCardControls model path

viewExpandedCardControls : Model -> CardPath -> Html Msg
viewExpandedCardControls model path = div [ class "controls" ]
    [ button
        [ class "expander", onClick (Collapse path) ]
        [ div [ class "sr-only" ] [ text "collapse" ]
        , div [ class "icon" ]    [ text "▼" ]
        ]
    ]

viewCollapsedCardControls : Model -> CardPath -> Html Msg
viewCollapsedCardControls model path = div [ class "controls" ]
    [ button
        [ class "expander", onClick (Expand path) ]
        [ div [ class "sr-only" ] [ text "expand" ]
        , div [ class "icon" ]    [ text "▶" ]
        ]
    ]

viewChildlessCardControls : Model -> CardPath -> Html Msg
viewChildlessCardControls model path = div [ class "controls" ]
    [ button
        [ class "disabled-expander", disabled True ]
        [ div [ class "sr-only" ] [ text "has no children" ]
        , div [ class "transparent-hack" ] [ text "." ]
        ]
    ]

viewMainMenu : Model -> Html Msg
viewMainMenu model = div [ class "main-menu" ]
    [ div [ class "button-bar" ]
        [ div [ class "button-group", class "button-group-general" ]
            [ button
                [ onClick NotImplementedMsg ]
                [ text "export data" ]
            , button
                [ onClick NotImplementedMsg ]
                [ text "import data" ]
            ]
        ]
    ]

viewError : Maybe ErrorMessage -> List (Html Msg)
viewError error = case error of
    Nothing -> []
    Just err -> [div
        [ class "error-box", onClick ClearError ]
        ( case err of
            NotImplemented -> [text "not implemented"]
            UnlinkWithoutParent -> [ text "trying to unlink item without parent" ]
        )]

view : Model -> Html Msg
view model =
    div []
        ( (viewError model.error) ++
        [ viewMainMenu model
        , div [ class "cards" ]
            [ viewCard model (NE.fromElement model.rootCard) model.cards
            ]
        ])

-- Helpers

isExpanded : Model -> CardPath -> Bool
isExpanded model path = PT.member path model.expanded

expand : CardPath -> Model -> (Model, Actions)
expand path model =
    let
        model1 = { model | expanded = PT.put path () model.expanded }
    in syncCards model1

collapse : CardPath -> Model -> (Model, Actions)
collapse path model =
    let
        model1 = { model | expanded = PT.drop path model.expanded }
    in syncCards model1

newState : UserState -> Model -> Model
newState state = updateState (\_ -> state)

updateState : (UserState -> UserState) -> Model -> Model
updateState f model =
    { model | state = f model.state }


saveCard : CardID -> Cards -> Actions
saveCard id cards = case Dict.get id cards of
    Nothing -> []
    Just card -> [SaveCard card]

insertChild : CardID -> Insertion -> Cards -> (Cards, CardPath)
insertChild id (mode, path) cards = case mode of
    FirstChild ->
        ( Cards.add (fetch (NE.head path) cards |> insertFirstChild id) cards
        , NE.cons id path )
    LastChild  ->
        ( Cards.add (fetch (NE.head path) cards |> insertLastChild id) cards
        , NE.cons id path )
    Before     ->
        ( Cards.add (fetch (getParent path) cards |> insertChildBefore (NE.head path) id) cards
        , NE.cons id (getParentPath path) )
    After      ->
        ( Cards.add (fetch (getParent path) cards |> insertChildAfter (NE.head path) id) cards
        , NE.cons id (getParentPath path) )

insertFirstChild : CardID -> Card -> Card
insertFirstChild childID card = { card | children = childID :: card.children }

insertLastChild : CardID -> Card -> Card
insertLastChild childID card = { card | children = card.children ++ [childID] }

insertChildBefore : CardID -> CardID -> Card -> Card
insertChildBefore ref childID card = { card | children = insertBefore ref childID card.children }

insertChildAfter : CardID -> CardID -> Card -> Card
insertChildAfter ref childID card = { card | children = insertAfter ref childID card.children }

addChildToCard : CardID -> CardID -> Cards -> Cards
addChildToCard childID parentID cards =
    case Dict.get parentID cards of
        Nothing -> cards
        Just oldParent -> let parent = { oldParent | children = childID :: oldParent.children } in
            Cards.add parent cards

delChildFromCard : CardID -> CardID -> Cards -> Cards
delChildFromCard cardID childID cards =
    case Dict.get cardID cards of
        Nothing -> cards
        Just oldParent -> let parent = { oldParent | children = List.filter (\c -> c /= childID) oldParent.children } in
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
        case isExpanded m path of
            True ->
                (List.foldr
                    (\child all -> Set.union all (neededCardsFrom (NE.cons child path) m)) Set.empty
                    (childrenOf m.cards (NE.head path)))
            False -> Set.empty

childrenOf : Cards -> CardID -> List CardID
childrenOf cards id = case Dict.get id cards of
    Nothing -> []
    Just card -> card.children

getParent : CardPath -> CardID
getParent path = case NE.tail path |> List.head of
    Just parent -> parent
    Nothing -> Debug.todo "looking for parent of root"

getParentPath : CardPath -> CardPath
getParentPath path = case NE.tail path |> NE.fromList of
    Just l  -> l
    Nothing -> Debug.todo "looking for parent of root"

setError : ErrorMessage -> Model -> Model
setError err model = { model | error = Just err }

clearError : Model -> Model
clearError model = { model | error = Nothing }

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
            CardEditing   -> 0.5
    in let
        colour = Color.hsl hue sat 0.8 |> Color.toCssString
    in
        style "background-color" colour

interpolate : Float -> Float -> Float -> Float
interpolate a b x = x * (b - a) + a

insertBefore : comparable -> comparable -> List comparable -> List comparable
insertBefore ref item list = case list of
    []      -> [item]
    x :: xs -> case x == ref of
        True  -> item :: x :: xs
        False -> x :: (insertBefore ref item xs)

insertAfter : comparable -> comparable -> List comparable -> List comparable
insertAfter ref item list = case list of
    []      -> [item]
    x :: xs -> case x == ref of
        True  -> x :: item :: xs
        False -> x :: (insertAfter ref item xs)

fetch : comparable -> Dict comparable v -> v
fetch k d =
    case Dict.get k d of
        Just v ->
            v

            -- here be dragons
        -- Nothing -> makeUndefined k
        Nothing -> Debug.todo "item not in dict"
