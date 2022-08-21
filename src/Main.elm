module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, src, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import Url exposing (Url)


type PomodoroType
    = Break
    | Work


type Second
    = Second Int


type Minute
    = Minute Int


min_to_sec : Minute -> Second
min_to_sec minute =
    case minute of
        Minute min ->
            Second (min * 60)


type alias Model =
    { set_work_time : Second
    , set_break_time : Second
    , work_time : Second
    , break_time : Second
    , active_timer : PomodoroType
    , key : Nav.Key
    }


type Msg
    = NoOp
    | Tick Time.Posix
    | Reset
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


custom_button : msg -> Html msg -> Html msg
custom_button msg children =
    button
        [ class "bg-black px-7 py-2 text-2xl text-white rounded-md"
        , onClick msg
        ]
        [ children
        ]


custom_input : Int -> msg -> Html msg
custom_input time msg =
    input
        [ onInput <| \_ -> msg
        , class "bg-indigo-100 border border-1 border-indigo-500 focus:ring hover:ring px-1 py-1 rounded-md"
        , type_ "number"
        , value <| String.fromInt time
        ]
        []


work_time : Second -> Html Msg
work_time set_work_time =
    let
        ( Minute min, Second sec ) =
            sec_to_tuple set_work_time
    in
    div []
        [ h3 [ class "text-center" ]
            [ text "Work Time"
            ]
        , div [ class "flex items-center py-3 space-x-2" ]
            [ div []
                [ div [] [ label [] [ text "Mins" ] ]
                , div [] [ custom_input min NoOp ]
                ]
            , div []
                [ div [] [ label [] [ text "Sec" ] ]
                , div [] [ custom_input sec NoOp ]
                ]
            ]
        ]


break_time : Second -> Html Msg
break_time set_break_time =
    let
        ( Minute min, Second sec ) =
            sec_to_tuple set_break_time
    in
    div []
        [ h3 [ class "text-center" ]
            [ text "Break Time"
            ]
        , div [ class "flex items-center py-3 space-x-2" ]
            [ div []
                [ div [] [ label [] [ text "Mins" ] ]
                , div [] [ custom_input min NoOp ]
                ]
            , div []
                [ div [] [ label [] [ text "Sec" ] ]
                , div [] [ custom_input sec NoOp ]
                ]
            ]
        ]


sec_to_tuple : Second -> ( Minute, Second )
sec_to_tuple second =
    case second of
        Second sec ->
            let
                min_int =
                    sec // 60

                sec_after_min =
                    (toFloat sec / 60 - toFloat min_int)
                        * 60
                        |> round
            in
            ( Minute min_int, Second sec_after_min )


sec_to_string : Second -> String
sec_to_string second =
    let
        ( Minute min, Second sec ) =
            sec_to_tuple second

        sec_str =
            if sec < 10 then
                "0" ++ String.fromInt sec

            else
                String.fromInt sec
    in
    String.fromInt min ++ ":" ++ sec_str


view : Model -> Browser.Document Msg
view model =
    { title = "counter"
    , body =
        [ div
            [ class "flex flex-col space-y-6 items-center mx-5 sm:mx-0 sm:mx-auto lg sm:max-w-xl lg:max-w-3xl mt-10 space-x-10" ]
            [ div []
                [ h1 [ class "text-5xl font-bold text-center" ] [ text "WORK TIMER" ]
                , h2 [ class "font-semibold text-[15rem]" ]
                    [ text <| sec_to_string model.work_time
                    ]
                ]
            , div [ class "felx justify-between space-x-10" ]
                [ custom_button NoOp (text "START")
                , custom_button Reset (text "RESET")
                ]
            , work_time model.set_work_time
            , break_time model.set_break_time
            ]
        ]
    }


default_work_time : Minute
default_work_time =
    Minute 10


default_break_time : Minute
default_break_time =
    Minute 5


update_second_by : Second -> Int -> Second
update_second_by second by =
    case second of
        Second s ->
            Second (s + by)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( { model
                | work_time = min_to_sec default_work_time
                , break_time = min_to_sec default_break_time
                , active_timer = Work
              }
            , Cmd.none
            )

        Tick new_time ->
            case model.active_timer of
                Break ->
                    let
                        new_sec =
                            update_second_by model.break_time -1
                    in
                    ( { model | break_time = new_sec }, Cmd.none )

                Work ->
                    ( { model | work_time = update_second_by model.work_time -1 }, Cmd.none )

        ChangedUrl url ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { set_work_time = min_to_sec default_work_time
      , set_break_time = min_to_sec default_break_time
      , work_time = min_to_sec default_work_time
      , break_time = min_to_sec default_break_time
      , active_timer = Work
      , key = key
      }
    , Cmd.none
    )


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
