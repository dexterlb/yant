module Main exposing (main)

import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url as Url
import Url.Parser as UP
import Url.Parser exposing ((</>))
import Browser
import Browser exposing (UrlRequest, Document)
import Browser.Navigation exposing (Key)
import Json.Encode as JE
import Json.Decode as JD
import Html exposing (Html, text, div)
import Html.Attributes exposing (class)

import Ui
import Storage
import Cards
import CardContent
import Settings
import Utils exposing (..)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }

-- MODEL


type alias Model =
    { url: Url
    , key: Key
    , ui:  Ui.Model
    }


init : JE.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key = let (ui, cmd, actions) = Ui.init (processJsonFlags flags) "root" in
    let model = emptyModel url key ui
        in (model, Cmd.batch [cmd, performUiActions model actions])


emptyModel : Url -> Key -> Ui.Model -> Model
emptyModel url key ui =
    { url = url
    , key = key
    , ui  = ui
    }

-- UPDATE


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest
    | UiMsg Ui.Msg
    | UiInput Ui.InputMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url -> init JE.null url model.key

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UiMsg uiMsg ->
            let (ui, cmd, actions) = Ui.update uiMsg model.ui
            in let model2 = { model | ui = ui }
            in
                (model2, Cmd.batch [Cmd.map UiMsg cmd, performUiActions model2 actions])

        UiInput input ->
            let (ui, cmd, actions) = Ui.pushMsg input model.ui
            in let model2 = { model | ui = ui }
            in
                (model2, Cmd.batch [Cmd.map UiMsg cmd, performUiActions model2 actions])


performUiActions : Model -> Ui.Actions -> Cmd Msg
performUiActions model actions = Cmd.batch <| List.map (performUiAction model) actions

performUiAction : Model -> Ui.Action -> Cmd Msg
performUiAction model action = case action of
    Ui.GetCard  mode id -> Storage.getCard  <| JE.object
        [ ("mode", Ui.encodeCardGetMode mode)
        , ("id", Cards.encodeCardID id)
        ]
    Ui.SaveCard card -> Storage.saveCard <| Cards.encodeCard   card
    Ui.RequestSettingsSave settings      -> Storage.saveSettings <| Settings.encode settings
    Ui.RequestAttachedFile               -> Storage.attachFile ()
    Ui.RequestAttachedFileDownload af    -> Storage.downloadAttachedFile <| CardContent.encodeAttachedFile af
    Ui.RequestDataExport      -> Storage.exportData ()
    Ui.RequestDataImport      -> Storage.importData ()
    Ui.RequestDataNuke        -> Storage.nukeData ()

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch
    [ Storage.gotCard (handleJson (UiInput << Ui.GotCard) Cards.decodeCard)
    , Storage.missingCard (handleJson (UiInput << Ui.MissingCard) JD.string)
    , Storage.attachedFile (handleJson (UiInput << Ui.ReceivedAttachedFile) CardContent.decodeAttachedFile)
    , Storage.reload (\_ -> UiInput Ui.Reload)
    ]

handleJson : (a -> Msg) -> JD.Decoder a -> JE.Value -> Msg
handleJson handler decoder v = case JD.decodeValue decoder v of
    Err err  -> crash (JD.errorToString err)
    Ok  data -> handler data

-- VIEW

view : Model -> Document Msg
view model = Document
    "notes"
    [ Html.map UiMsg <| Ui.view model.ui ]

-- Flags

processJsonFlags : JE.Value -> Maybe Ui.Flags
processJsonFlags val = case JD.decodeValue Ui.decodeFlags val of
    Ok flags -> Just flags
    Err _    -> Nothing
