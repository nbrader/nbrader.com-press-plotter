module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, h1, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Svg exposing (Svg, svg, rect, text as svgText)
import Svg.Attributes as SvgA
import Time exposing (Posix, every, now)
import Task exposing (Task)
import List exposing (sum)


-- Model

type alias Event =
    { startX : Float
    , length : Float
    , color : String
    }

type alias Model =
    { events : List Event
    , recording : Bool
    , currentStart : Maybe Posix
    , currentValue : Float
    , lastTime : Maybe Posix
    }

initialModel : Model
initialModel =
    { events = []
    , recording = False
    , currentStart = Nothing
    , currentValue = 0
    , lastTime = Nothing
    }


-- Messages

type Msg
    = StartRecording
    | StopRecording
    | ClearRecording
    | SetValue Float
    | Tick Posix
    | UpdateCurrentTime Posix


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartRecording ->
            ( { initialModel | recording = True }, Task.perform UpdateCurrentTime now )

        StopRecording ->
            ( { model | recording = False, currentStart = Nothing }, Cmd.none )

        ClearRecording ->
            ( { model | events = [], currentValue = 0 }, Cmd.none )

        SetValue value ->
            if model.recording then
                let
                    (updatedEvents, newCurrentStart) =
                        case (model.lastTime, model.currentStart) of
                            (Just lastTime, Just currentStart) ->
                                let
                                    elapsed = (toFloat (Time.posixToMillis lastTime - Time.posixToMillis currentStart)) / 100
                                    newEvent = { startX = sum (List.map .length model.events), length = elapsed, color = if model.currentValue == 1 then "blue" else "green" }
                                in
                                (newEvent :: model.events, Just lastTime)

                            _ ->
                                (model.events, model.currentStart)
                in
                ( { model | currentValue = value, events = updatedEvents, currentStart = newCurrentStart }, Cmd.none )
            else
                ( model, Cmd.none )

        Tick time ->
            if model.recording then
                ( { model | lastTime = Just time }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateCurrentTime time ->
            ( { model | recording = True, currentStart = Just time, lastTime = Just time, events = [] }, Cmd.none )


-- Helper Functions

getTotalTime : Model -> Float
getTotalTime model =
    let
        eventsTime = sum (List.map .length model.events)
        currentTime =
            case (model.currentStart, model.lastTime) of
                (Just start, Just lastTime) ->
                    (toFloat (Time.posixToMillis lastTime - Time.posixToMillis start)) / 100
                _ ->
                    0
    in
    eventsTime + currentTime


timeAxisMarkers : Float -> List (Svg Msg)
timeAxisMarkers totalTime =
    let
        maxSeconds = ceiling (totalTime / 10)
        secondMarkers = List.range 0 maxSeconds
    in
    List.concatMap (\sec ->
        let
            xPos = toFloat sec * 10
        in
        [ Svg.line
            [ SvgA.x1 (String.fromFloat xPos)
            , SvgA.y1 "350"
            , SvgA.x2 (String.fromFloat xPos)
            , SvgA.y2 "360"
            , SvgA.stroke "#666"
            , SvgA.strokeWidth "1"
            ] []
        , svgText
            [ SvgA.x (String.fromFloat xPos)
            , SvgA.y "375"
            , SvgA.fontSize "12"
            , SvgA.textAnchor "middle"
            , SvgA.fill "#666"
            ]
            [ Svg.text (String.fromInt sec ++ "s") ]
        ]
    ) secondMarkers


-- View

view : Model -> Html Msg
view model =
    let
        totalTime = getTotalTime model
        svgWidth = max 800 (totalTime + 100)
        recordingIndicator =
            if model.recording then
                div [ style "display" "inline-block", style "margin-left" "10px", style "color" "red", style "font-weight" "bold" ]
                    [ text "⬤ RECORDING" ]
            else
                div [ style "display" "inline-block", style "margin-left" "10px", style "color" "#999" ]
                    [ text "○ Not Recording" ]
    in
    div [ style "padding" "20px", style "font-family" "sans-serif" ]
        [ h1 [] [ text "Press Plotter" ]
        , p [] [ text "Visualize button press patterns over time" ]
        , div [ style "margin" "20px 0" ]
            [ button [ onClick StartRecording, style "margin-right" "10px", style "padding" "10px 20px" ] [ text "Start Recording" ]
            , button [ onClick StopRecording, style "margin-right" "10px", style "padding" "10px 20px" ] [ text "Stop Recording" ]
            , button [ onClick ClearRecording, style "margin-right" "10px", style "padding" "10px 20px", style "background-color" "#ff9800", style "color" "white", style "border" "none" ] [ text "Clear" ]
            , button [ onMouseDown (SetValue 1), onMouseUp (SetValue 0), style "padding" "10px 20px", style "background-color" (if model.currentValue == 1 then "#2196F3" else "#4CAF50"), style "color" "white", style "border" "none", style "cursor" "pointer" ]
                [ text "Hold Me to Record Press" ]
            , recordingIndicator
            ]
        , div [ style "margin" "20px 0" ]
            [ div [ style "display" "inline-block", style "margin-right" "20px" ]
                [ div [ style "display" "inline-block", style "width" "20px", style "height" "20px", style "background-color" "blue", style "margin-right" "5px", style "vertical-align" "middle" ] []
                , text "Pressed"
                ]
            , div [ style "display" "inline-block", style "margin-right" "20px" ]
                [ div [ style "display" "inline-block", style "width" "20px", style "height" "20px", style "background-color" "green", style "margin-right" "5px", style "vertical-align" "middle" ] []
                , text "Released"
                ]
            , div [ style "display" "inline-block", style "font-weight" "bold" ]
                [ text ("Duration: " ++ String.fromFloat (totalTime / 10) ++ "s") ]
            ]
        , div [ style "overflow-x" "auto", style "margin" "20px 0" ]
            [ svg [ SvgA.width (String.fromFloat svgWidth), SvgA.height "400", SvgA.style "border: 1px solid #ccc; background-color: #f9f9f9;" ]
                (List.concatMap eventToRectangles model.events ++ [currentRectangle model] ++ timeAxisMarkers totalTime)
            ]
        ]


eventToRectangles : Event -> List (Svg Msg)
eventToRectangles event =
    [ rect [ SvgA.x (String.fromFloat event.startX), SvgA.y "50", SvgA.width (String.fromFloat event.length), SvgA.height "300", SvgA.fill event.color ] [] ]


currentRectangle : Model -> Svg Msg
currentRectangle model =
    case (model.currentStart, model.lastTime) of
        (Just start, Just lastTime) ->
            let
                elapsed = (toFloat (Time.posixToMillis lastTime - Time.posixToMillis start)) / 100
                color = if model.currentValue == 1 then "blue" else "green"
            in
            rect [ SvgA.x (String.fromFloat (sum (List.map .length model.events))), SvgA.y "50", SvgA.width (String.fromFloat elapsed), SvgA.height "300", SvgA.fill color ] []

        _ ->
            rect [] []


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    every 100 Tick


-- Init

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


-- Main

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
