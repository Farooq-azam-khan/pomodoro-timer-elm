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
    | PauseWork
    | PauseBreak


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
    { set_work_time : ( Minute, Second )
    , set_break_time : ( Minute, Second )
    , work_time : Second
    , break_time : Second
    , active_timer : PomodoroType
    , key : Nav.Key
    }


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


custom_button : msg -> Html msg -> Html msg
custom_button msg children =
    button
        [ class "bg-black px-7 py-2 text-2xl text-white rounded-md"
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


work_time : ( Minute, Second ) -> Html Msg
work_time ( set_min, set_sec ) =
    -- set_work_time =
    let
        ( Minute min, Second sec ) =
            ( set_min, set_sec )

        -- sec_to_tuple set_work_time
    in
    div []
        [ h3 [ class "text-center" ]
            [ text "Work Time"
            ]
        , div [ class "flex items-center py-3 space-x-2" ]
            [ div []
                [ div [] [ label [] [ text "Mins" ] ]
                , div []
                    [ custom_input min (\val -> UpdateWorkTimeMinute (Minute (Maybe.withDefault 0 (String.toInt val)))) ]
                ]
            , div []
                [ div [] [ label [] [ text "Sec" ] ]
                , div [] [ custom_input sec (\val -> UpdateWorkTimeSecond (Second (Maybe.withDefault 0 (String.toInt val)))) ]
                ]
            ]
        ]


break_time : ( Minute, Second ) -> Html Msg
break_time ( Minute min, Second sec ) =
    -- set_break_time =
    div []
        [ h3 [ class "text-center" ]
            [ text "Break Time"
            ]
        , div [ class "flex items-center py-3 space-x-2" ]
            [ div []
                [ div [] [ label [] [ text "Mins" ] ]
                , div [] [ custom_input min (\val -> UpdateBreakTimeMinute (Minute (Maybe.withDefault 0 (String.toInt val)))) ]
                ]
            , div []
                [ div [] [ label [] [ text "Sec" ] ]
                , div []
                    [ custom_input sec (\val -> UpdateBreakTimeSecond (Second (Maybe.withDefault 0 (String.toInt val))))
                    ]
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
                [ case model.active_timer of
                    Work ->
                        h1 [ class "text-5xl font-bold text-center" ] [ text "WORK TIMER" ]

                    Break ->
                        h1 [ class "text-5xl font-bold text-center" ] [ text "BREAK TIMER" ]

                    PauseWork ->
                        h1 [ class "text-5xl font-bold text-center" ] [ text "WORK TIMER" ]

                    PauseBreak ->
                        h1 [ class "text-5xl font-bold text-center" ] [ text "BREAK TIMER" ]
                , case model.active_timer of
                    Work ->
                        h2 [ class "font-semibold text-[15rem]" ]
                            [ text <| sec_to_string model.work_time
                            ]

                    PauseWork ->
                        h2 [ class "font-semibold text-[15rem]" ]
                            [ text <| sec_to_string model.work_time
                            ]

                    Break ->
                        h2 [ class "font-semibold text-[15rem]" ]
                            [ text <| sec_to_string model.break_time
                            ]

                    PauseBreak ->
                        h2 [ class "font-semibold text-[15rem]" ]
                            [ text <| sec_to_string model.break_time
                            ]
                ]
            , div [ class "felx justify-between space-x-10" ]
                [ case model.active_timer of
                    Work ->
                        custom_button ToggleStartButton (text "PAUSE")

                    PauseWork ->
                        custom_button ToggleStartButton (text "START")

                    Break ->
                        custom_button ToggleStartButton (text "PAUSE")

                    PauseBreak ->
                        custom_button ToggleStartButton (text "Start")
                , custom_button Reset (text "RESET")
                ]
            , work_time model.set_work_time
            , break_time model.set_break_time
            ]
        ]
    }


default_work_time : ( Minute, Second )
default_work_time =
    ( Minute 10, Second 0 )


default_break_time : ( Minute, Second )
default_break_time =
    ( Minute 5, Second 0 )


update_second_by : Second -> Int -> Second
update_second_by second by =
    case second of
        Second s ->
            Second (s + by)


tuple_to_sec : ( Minute, Second ) -> Second
tuple_to_sec ( min, Second sec ) =
    let
        m =
            case min_to_sec min of
                Second s ->
                    s
    in
    Second (m + sec)


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
                , active_timer = PauseWork
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
                , active_timer = PauseWork
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
                , active_timer = PauseBreak
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
                , active_timer = PauseBreak
              }
            , Cmd.none
            )

        ToggleStartButton ->
            case model.active_timer of
                PauseWork ->
                    ( { model | active_timer = Work }, Cmd.none )

                PauseBreak ->
                    ( { model | active_timer = Break }, Cmd.none )

                Work ->
                    ( { model | active_timer = PauseWork }, Cmd.none )

                Break ->
                    ( { model | active_timer = PauseBreak }, Cmd.none )

        Reset ->
            ( { model
                | work_time = tuple_to_sec model.set_work_time -- (min_to_sec <| Tuple.first model.set_work_time) + Tuple.second model.set_break_time
                , break_time = tuple_to_sec model.set_break_time
                , active_timer = PauseWork
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
                    case new_sec of
                        Second s ->
                            if s < 0 then
                                ( { model
                                    | break_time = tuple_to_sec model.set_break_time
                                    , active_timer = Work
                                    , work_time = tuple_to_sec model.set_work_time
                                  }
                                , Cmd.none
                                )

                            else
                                ( { model | break_time = new_sec }, Cmd.none )

                Work ->
                    let
                        new_sec =
                            update_second_by model.work_time -1
                    in
                    case new_sec of
                        Second s ->
                            if s < 0 then
                                ( { model
                                    | break_time = tuple_to_sec model.set_break_time
                                    , work_time = tuple_to_sec model.set_work_time
                                    , active_timer = Break
                                  }
                                , Cmd.none
                                )

                            else
                                ( { model | work_time = new_sec }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
    case model.active_timer of
        Work ->
            Time.every 1000 Tick

        Break ->
            Time.every 1000 Tick

        _ ->
            Sub.none


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { set_work_time = default_work_time
      , set_break_time = default_break_time
      , work_time = min_to_sec <| Tuple.first default_work_time
      , break_time = min_to_sec <| Tuple.first default_break_time
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
