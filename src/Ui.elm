module Ui exposing (Model, Msg, init, update, view, InputMsg(..), Action(..), Actions, pushMsg)

import Html exposing (Html, div, text, button, textarea)
import Html.Attributes exposing (class, value, placeholder, style, disabled)
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
import Utils
import CardContent as CardContent exposing (CardContent)


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
    , content: CardContent
    }

type alias Clipboard = (CardPath, ClipboardState)

type ClipboardState
    = Move
    | Link

type InsertMode
    = FirstChild
    | LastChild
    | Before
    | After

type alias Insertion = (InsertMode, CardPath)

type UserState
    = None (Maybe Clipboard)
    | Selected CardPath (Maybe Clipboard)
    | Editing EditContext
    | PendingEdit CardPath

type CardState
    = CardNone
    | CardSelected
    | CardEditing

type Msg
    = TextChanged     String
    | SelectCard      CardPath
    | DeselectCard
    | EditCard        CardPath
    | UnlinkCard      CardPath
    | AddChild        Insertion
    | Expand          CardPath
    | Collapse        CardPath
    | AddChildWithID  Insertion CardID
    | SaveEdit
    | CancelEdit
    | ContentMsg      CardID CardContent.Msg
    | NotImplemented
    | SetClipboard    (Maybe Clipboard)
    | PasteChild      Insertion Clipboard

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
                { state = None Nothing
                , expanded = PT.empty
                }
            }
    in let
        (model1, actions) = syncCards model
    in
        (model1, Cmd.none, actions)

update : Msg -> Model -> ( Model, Cmd Msg, Actions )
update msg model = let oldContext = model.context in case msg of
    TextChanged text -> ( updateEditText text model, Cmd.none, [])
    SelectCard path  -> ( newState (Selected path (clipboardOf model.context.state)) model, Cmd.none, [] )
    DeselectCard     -> ( newState (None (clipboardOf model.context.state)) model, Cmd.none, [] )
    EditCard path    ->
        ( editMode path model, Cmd.none, [])
    UnlinkCard path -> case NE.tail path |> List.head of
        Nothing -> Debug.todo "unlinking card without parent"
        Just parent ->
            let model1 = { model | cards = delChildFromCard parent (NE.head path) model.cards }
            in
                ( model1, Cmd.none, (saveCard parent model1.cards) )
    SaveEdit -> case model.context.state of
        Editing ectx -> let (cards, cmd, actions) = editCard ectx model.cards in
            ( { model | cards = cards, context = { oldContext | state = None Nothing } }, cmd, actions )
        _ -> (model, Cmd.none, [])
    CancelEdit -> case model.context.state of
        Editing ectx ->
            ( { model | context = { oldContext | state = None Nothing } }, Cmd.none, [] )
        _ -> (model, Cmd.none, [])
    AddChild ins -> ( model, randomID (AddChildWithID ins), [])
    AddChildWithID ins id ->
        let (newCards, newPath) = insertChild id ins model.cards
        in let
            model1 = { model | cards = newCards }
            parentPath = getParentPath newPath
        in let (model2, actions1) = expand parentPath model1
        in let
            (model3, actions2) = ( model2, (saveCard (NE.head parentPath) model2.cards) ++ actions1 )
        in
            ( newState (PendingEdit newPath) model3, Cmd.none, actions2 )

    PasteChild ins (oldPath, Move) ->
        let
            (model1, cmd1, actions1) = update (UnlinkCard oldPath) model
        in let
            (model2, cmd2, actions2) = update (AddChildWithID ins (NE.head oldPath)) model1
        in  (model2, Cmd.batch [cmd1, cmd2], actions1 ++ actions2)

    PasteChild ins (oldPath, Link) ->
        update (AddChildWithID ins (NE.head oldPath)) model

    Expand path ->
        let (model1, actions) = expand path model
        in
            ( model1, Cmd.none, actions )
    Collapse path ->
        let (model1, actions) = collapse path model
        in
            ( model1, Cmd.none, actions )

    SetClipboard clip ->
        let model1 = case model.context.state of
                        Selected path _ -> newState (Selected path clip) model
                        None _          -> newState (None clip) model
                        _               -> model
        in
            ( model1, Cmd.none, [] )

    ContentMsg _ CardContent.Foo -> (model, Cmd.none, [])

    NotImplemented -> Debug.todo "not implemented"

editMode : CardPath -> Model -> Model
editMode path model = case Dict.get (NE.head path) model.cards of
    Nothing -> model
    Just card -> newState (Editing { path = path, content = card.content }) model

pushMsg : InputMsg -> Model -> ( Model, Cmd Msg, Actions )
pushMsg inMsg model = case inMsg of
    GotCard card ->
        let model1 = { model | cards = Cards.add card model.cards }
        in let
            model2 = case model1.context.state of
                        PendingEdit path -> case (NE.head path) == card.id of
                            True -> editMode path model1
                            False -> model1
                        _ -> model1
        in
            ( model2 , Cmd.none, [] )


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
        None _ -> viewPlainCardBody ctx path card
        PendingEdit _ -> viewPlainCardBody ctx path card
        Editing ectx -> case ectx.path == path of
            True -> viewEditingCardBody ctx path card ectx
            False -> viewPlainCardBody ctx path card
        Selected spath clip -> case spath == path of
            True -> viewSelectedCardBody ctx path card clip
            False -> viewPlainCardBody ctx path card



viewEditingCardBody : Context -> CardPath -> Card -> EditContext -> Html Msg
viewEditingCardBody ctx path card ectx = div
    [ class "card-body", class "editing", cardColour path CardEditing ]
    [ viewCardControls ctx path card
    , div [ class "card-vbox" ]
        [ textarea
            [ value ectx.content.text
            , placeholder "enter some note text"
            , HE.onInput TextChanged
            ] []
        , div [ class "button-bar" ]
            [ button
                [ onClick SaveEdit ]
                [ text "save" ]
            , button
                [ onClick CancelEdit ]
                [ text "cancel" ]
            ]
        ]
    ]

viewPlainCardBody : Context -> CardPath -> Card -> Html Msg
viewPlainCardBody ctx path card = div
    [ class "card-body", class "plain"
    , onClick (SelectCard path)
    , cardColour path CardNone
    ]
    [ viewCardControls ctx path card
    , div [ class "card-vbox" ]
        [ viewCardContent card
        ]
    ]

viewSelectedCardBody : Context -> CardPath -> Card -> Maybe Clipboard -> Html Msg
viewSelectedCardBody ctx path card clip = div
    [ class "card-body", class "selected", cardColour path CardSelected ]

    [ viewCardControls ctx path card
    , div [ class "card-vbox" ]
        [ viewCardButtonBar path card clip
        , viewCardContent card
        ]
    ]

viewCardContent : Card -> Html Msg
viewCardContent card
    =  CardContent.render card.content
    |> Html.map (ContentMsg card.id)


viewCardButtonBar : CardPath -> Card -> Maybe Clipboard -> Html Msg
viewCardButtonBar path card clip = case clip of
    Nothing    -> viewCardToolbar path card
    Just clips -> viewCardPasteBar path card clips

viewCardPasteBar : CardPath -> Card -> (CardPath, ClipboardState) -> Html Msg
viewCardPasteBar path card clip = div [ class "button-bar" ] (
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

viewCardToolbar : CardPath -> Card -> Html Msg
viewCardToolbar path card = div [ class "button-bar" ] (
    [ button
        [ onClick DeselectCard ]
        [ text "deselect" ]
    , button
        [ onClick (EditCard path) ]
        [ text "edit" ]
    , button
        [ onClick (SetClipboard (Just (path, Move))) ]
        [ text "move" ]
    , button
        [ onClick (SetClipboard (Just (path, Link))) ]
        [ text "link" ]
    , button
        [ onClick (AddChild (FirstChild, path)) ]
        [ text "add child" ]
    ] ++ case NE.tail path |> List.isEmpty of
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

viewCardControls : Context -> CardPath -> Card -> Html Msg
viewCardControls ctx path card =
    case List.isEmpty card.children of
        True  -> viewChildlessCardControls  ctx path
        False ->
            case isExpanded ctx path of
                True  -> viewExpandedCardControls  ctx path
                False -> viewCollapsedCardControls ctx path

viewExpandedCardControls : Context -> CardPath -> Html Msg
viewExpandedCardControls ctx path = div [ class "controls" ]
    [ button
        [ class "expander", onClick (Collapse path) ]
        [ div [ class "sr-only" ] [ text "collapse" ]
        , div [ class "icon" ]    [ text "▼" ]
        ]
    ]

viewCollapsedCardControls : Context -> CardPath -> Html Msg
viewCollapsedCardControls ctx path = div [ class "controls" ]
    [ button
        [ class "expander", onClick (Expand path) ]
        [ div [ class "sr-only" ] [ text "expand" ]
        , div [ class "icon" ]    [ text "▶" ]
        ]
    ]

viewChildlessCardControls : Context -> CardPath -> Html Msg
viewChildlessCardControls ctx path = div [ class "controls" ]
    [ button
        [ class "disabled-expander", disabled True ]
        [ div [ class "sr-only" ] [ text "has no children" ]
        , div [ class "transparent-hack" ] [ text "." ]
        ]
    ]

viewMainMenu : Context -> Html Msg
viewMainMenu ctx = div [ class "main-menu" ]
    [ div [ class "button-bar" ]
        [ button
            [ onClick NotImplemented ]
            [ text "export data" ]
        , button
            [ onClick NotImplemented ]
            [ text "import data" ]
        ]
    ]


view : Model -> Html Msg
view { context, cards, rootCard } =
    div []
        [ viewMainMenu context
        , div [ class "cards" ]
            [ viewCard context (NE.fromElement rootCard) cards
            ]
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

-- todo: rewrite this to use updateState
updateEditContext : (EditContext -> EditContext) -> Model -> Model
updateEditContext f model = let oldContext = model.context in case model.context.state of
    Editing ectx -> { model | context = { oldContext | state = Editing (f ectx) } }
    _            -> model

updateEditText : String -> Model -> Model
updateEditText text = updateEditContext
    (\ctx -> let content = ctx.content in
        { ctx | content = { content | text = text } }
    )

newState : UserState -> Model -> Model
newState state = updateState (\_ -> state)

updateState : (UserState -> UserState) -> Model -> Model
updateState f model = let oldContext = model.context in
    { model | context = { oldContext | state = f oldContext.state } }


editCard : EditContext -> Cards -> (Cards, Cmd Msg, Actions)
editCard ectx cards = case Dict.get (NE.head ectx.path) cards of
    Nothing -> (cards, Cmd.none, [])
    Just oldCard -> let card = { oldCard | content = ectx.content } in
        (Cards.add card cards, Cmd.none, [SaveCard card])

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

getParent : CardPath -> CardID
getParent path = case NE.tail path |> List.head of
    Just parent -> parent
    Nothing -> Debug.todo "looking for parent of root"

getParentPath : CardPath -> CardPath
getParentPath path = case NE.tail path |> NE.fromList of
    Just l  -> l
    Nothing -> Debug.todo "looking for parent of root"

clipboardOf : UserState -> Maybe Clipboard
clipboardOf state = case state of
    None clip       -> clip
    Selected _ clip -> clip
    _               -> Nothing

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

onClick : msg -> Html.Attribute msg
onClick msg = HE.stopPropagationOn "click" (JD.succeed (msg, True))

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
