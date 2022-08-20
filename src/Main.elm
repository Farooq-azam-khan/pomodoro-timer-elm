module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, src, target)
import Html.Events exposing (onClick)
import Url exposing (Url)


type alias Model =
    { count : Int, key : Nav.Key }


type Msg
    = Increment
    | Decrement
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


view : Model -> Browser.Document Msg
view model =
    { title = "counter"
    , body =
        [ div
            [ class "mx-5 sm:mx-0 sm:mx-auto lg sm:max-w-xl lg:max-w-3xl mt-10 space-x-10" ]
            [ button [ class "bg-black px-5 py-1 text-white rounded-md", onClick Decrement ] [ text "-" ]
            , span [] [ model.count |> String.fromInt |> text ]
            , button [ class "bg-black px-5 py-1 text-white rounded-md", onClick Increment ] [ text "+" ]
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        ChangedUrl url ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { count = 0, key = key }, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
