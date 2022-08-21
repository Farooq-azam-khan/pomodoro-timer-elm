module Main exposing (Minute(..), Model, Msg(..), PomodoroType(..), Second(..), TimerActivity(..), break_time, custom_button, custom_input, default_break_time, default_work_time, init, main, map_s, min_to_sec, sec_to_string, sec_to_tuple, subscriptions, tuple_to_sec, update, update_second_by, view, work_time)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, src, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Time
import Url exposing (Url)


type Msg
    = NoOp
    | Tick Time.Posix
    | UpdateWorkTimeMinute Minute
    | UpdateWorkTimeSecond Second
    | UpdateBreakTimeSecond Second
    | UpdateBreakTimeMinute Minute
    | ToggleStartButton
    | Reset
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


type alias Model =
    { set_work_time : SetTime
    , set_break_time : SetTime
    , work_time : Second
    , break_time : Second
    , active_timer : PomodoroType
    , key : Nav.Key
    }


type TimerActivity
    = Paused
    | Active


type alias SetTime =
    ( Minute, Second )


type PomodoroType
    = Break TimerActivity
    | Work TimerActivity


type Second
    = Second Int


type Minute
    = Minute Int


map_s : (Int -> b) -> Second -> b
map_s f second =
    case second of
        Second s ->
            f s


map_min : (Int -> b) -> Minute -> b
map_min f minute =
    case minute of
        Minute min ->
            f min


default_work_time : SetTime
default_work_time =
    ( Minute 10, Second 0 )


default_break_time : SetTime
default_break_time =
    ( Minute 5, Second 0 )


update_second_by : Second -> Int -> Second
update_second_by second by =
    map_s (\int_s -> Second (int_s + by)) second


tuple_to_sec : ( Minute, Second ) -> Second
tuple_to_sec ( min, Second sec ) =
    map_s (\int_s -> Second (int_s + sec)) (min_to_sec min)


min_to_sec : Minute -> Second
min_to_sec minute =
    map_min (\int_min -> Second (int_min * 60)) minute


update_time_wrapper :
    Minute
    -> (String -> Msg)
    -> Second
    -> (String -> Msg)
    -> Html Msg
update_time_wrapper (Minute min) min_parser (Second sec) sec_parser =
    div [ class "flex items-center py-3 space-x-2" ]
        [ div [ class "flex items-center space-x-2" ]
            [ label [] [ text "Mins" ]
            , custom_input
                min
                min_parser
            ]
        , div [ class "flex items-center space-x-2" ]
            [ label [] [ text "Sec" ]
            , custom_input
                sec
                sec_parser
            ]
        ]


work_time : ( Minute, Second ) -> Html Msg
work_time ( min, sec ) =
    let
        parse_input =
            \val ->
                val
                    |> String.toInt
                    |> Maybe.withDefault 0
    in
    div []
        [ h3 [ class "text-center text-lg mt-3" ]
            [ text "Work Time"
            ]
        , update_time_wrapper
            min
            (\val ->
                parse_input val
                    |> Minute
                    |> UpdateWorkTimeMinute
            )
            sec
            (\val ->
                parse_input val
                    |> Second
                    |> UpdateWorkTimeSecond
            )
        ]


break_time : ( Minute, Second ) -> Html Msg
break_time ( min, sec ) =
    let
        parse_input =
            \val -> val |> String.toInt |> Maybe.withDefault 0
    in
    div []
        [ h3
            [ class "text-center text-lg mt-3" ]
            [ text "Break Time"
            ]
        , update_time_wrapper
            min
            (\val ->
                parse_input val
                    |> Minute
                    |> UpdateBreakTimeMinute
            )
            sec
            (\val ->
                parse_input val
                    |> Second
                    |> UpdateBreakTimeSecond
            )
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
    case sec_to_tuple second of
        ( Minute min, Second sec ) ->
            let
                sec_str =
                    if sec < 10 then
                        "0" ++ String.fromInt sec

                    else
                        String.fromInt sec

                min_str =
                    if min < 10 then
                        "0" ++ String.fromInt min

                    else
                        String.fromInt min
            in
            min_str ++ ":" ++ sec_str


view : Model -> Browser.Document Msg
view model =
    { title = "counter"
    , body =
        [ div
            [ class "flex flex-col space-y-6 items-center mx-5 sm:mx-0 sm:mx-auto lg sm:max-w-xl lg:max-w-3xl mt-10 space-x-10" ]
            [ div []
                [ case model.active_timer of
                    Work _ ->
                        h1 [ class "text-5xl font-bold text-center" ] [ text "WORK TIMER" ]

                    Break _ ->
                        h1 [ class "text-5xl font-bold text-center" ] [ text "BREAK TIMER" ]
                , case model.active_timer of
                    Work _ ->
                        h2 [ class "font-semibold text-[15rem]" ]
                            [ text <| sec_to_string model.work_time
                            ]

                    Break _ ->
                        h2 [ class "font-semibold text-[15rem]" ]
                            [ text <| sec_to_string model.break_time
                            ]
                ]
            , div [ class "felx justify-between space-x-10" ]
                [ case model.active_timer of
                    Work Active ->
                        custom_button ToggleStartButton (text "PAUSE")

                    Work Paused ->
                        custom_button ToggleStartButton (text "START")

                    Break Active ->
                        custom_button ToggleStartButton (text "PAUSE")

                    Break Paused ->
                        custom_button ToggleStartButton (text "Start")
                , custom_button Reset (text "RESET")
                ]
            , work_time model.set_work_time
            , break_time model.set_break_time
            ]
        ]
    }


custom_button : msg -> Html msg -> Html msg
custom_button msg children =
    button
        [ class "bg-indigo-700 px-7 py-2 text-2xl text-white rounded-md"
        , onClick msg
        ]
        [ children
        ]


custom_input : Int -> (String -> msg) -> Html msg
custom_input time msg =
    input
        [ onInput msg
        , class "bg-indigo-100 border border-1 border-indigo-500 focus:ring hover:ring px-1 py-1 rounded-md"
        , type_ "number"
        , value <| String.fromInt time
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateWorkTimeMinute min ->
            let
                new_set_worktime =
                    ( min, Tuple.second model.set_work_time )
            in
            ( { model
                | set_work_time = new_set_worktime
                , work_time = tuple_to_sec new_set_worktime
              }
            , Cmd.none
            )

        UpdateWorkTimeSecond sec ->
            let
                new_set_work_time =
                    ( Tuple.first model.set_work_time, sec )
            in
            ( { model
                | set_work_time = new_set_work_time
                , work_time = tuple_to_sec new_set_work_time
              }
            , Cmd.none
            )

        UpdateBreakTimeMinute min ->
            let
                new_set_break_time =
                    ( min, Tuple.second model.set_break_time )
            in
            ( { model
                | set_break_time = new_set_break_time
                , break_time = tuple_to_sec new_set_break_time
              }
            , Cmd.none
            )

        UpdateBreakTimeSecond sec ->
            let
                new_set_break_time =
                    ( Tuple.first model.set_work_time, sec )
            in
            ( { model
                | set_break_time = new_set_break_time
                , break_time = tuple_to_sec new_set_break_time
              }
            , Cmd.none
            )

        ToggleStartButton ->
            case model.active_timer of
                Work Paused ->
                    ( { model | active_timer = Work Active }, Cmd.none )

                Break Paused ->
                    ( { model | active_timer = Break Active }, Cmd.none )

                Work Active ->
                    ( { model | active_timer = Work Paused }, Cmd.none )

                Break Active ->
                    ( { model | active_timer = Break Paused }, Cmd.none )

        Reset ->
            ( { model
                | work_time = tuple_to_sec model.set_work_time
                , break_time = tuple_to_sec model.set_break_time
                , active_timer = Work Paused
              }
            , Cmd.none
            )

        Tick new_time ->
            case model.active_timer of
                Break Active ->
                    let
                        new_sec =
                            update_second_by model.break_time -1
                    in
                    if is_sec_below_zero new_sec then
                        ( { model
                            | break_time = tuple_to_sec model.set_break_time
                            , active_timer = Work Active
                            , work_time = tuple_to_sec model.set_work_time
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | break_time = new_sec }, Cmd.none )

                Work Active ->
                    let
                        new_sec =
                            update_second_by model.work_time -1
                    in
                    if is_sec_below_zero new_sec then
                        ( { model
                            | break_time = tuple_to_sec model.set_break_time
                            , work_time = tuple_to_sec model.set_work_time
                            , active_timer = Break Active
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | work_time = new_sec }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


is_sec_below_zero : Second -> Bool
is_sec_below_zero (Second s) =
    if s < 0 then
        True

    else
        False


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.active_timer of
        Work Active ->
            Time.every 1000 Tick

        Break Active ->
            Time.every 1000 Tick

        _ ->
            Sub.none


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { set_work_time = default_work_time
      , set_break_time = default_break_time
      , work_time = min_to_sec <| Tuple.first default_work_time
      , break_time = min_to_sec <| Tuple.first default_break_time
      , active_timer = Work Paused
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
