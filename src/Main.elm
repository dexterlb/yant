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
    { url : Url
    , key: Key
    }


init : JE.Value -> Url -> Key -> ( Model, Cmd Msg )
init _ url key = let model = emptyModel url key
    in (model, startCommand model)


startCommand : Model -> Cmd Msg
startCommand model =
    Cmd.batch
        [
        ]


emptyModel : Url -> Key -> Model
emptyModel url key =
    { url = url
    , key = key
    }

-- UPDATE


type Msg
    = UrlChanged Url
    | UrlRequested UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url -> let newModel = emptyModel url model.key in
            ( newModel, startCommand newModel )

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

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch
    [
    ]



-- VIEW

view : Model -> Document Msg
view model = Document
    "notes"
    [ bodyView model ]

bodyView : Model -> Html Msg
bodyView model =
    div [] [ text "reconnecting" ]
