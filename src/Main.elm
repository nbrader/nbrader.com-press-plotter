module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, h1, p)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Svg exposing (Svg, svg, rect, text as svgText)
import Svg.Attributes as SvgA
import Time exposing (Posix, every, now)
import Task exposing (Task)
import List exposing (sum)


-- Constants

config =
    { tickIntervalMs = 100
    , msToPixels = 100  -- 100ms = 1 pixel, so 1 second = 10 pixels
    , timelineYStart = 50
    , timelineHeight = 300
    , timeAxisY = 350
    , timeAxisTickHeight = 10
    , timeAxisLabelY = 375
    , minSvgWidth = 800
    , svgHeight = 400
    , svgPadding = 100
    }


-- Model

type ButtonState
    = Pressed
    | Released


type alias Event =
    { startX : Float
    , length : Float
    , state : ButtonState
    }

type alias Model =
    { events : List Event
    , recording : Bool
    , currentStart : Maybe Posix
    , currentState : ButtonState
    , lastTime : Maybe Posix
    }

initialModel : Model
initialModel =
    { events = []
    , recording = False
    , currentStart = Nothing
    , currentState = Released
    , lastTime = Nothing
    }


-- Messages

type Msg
    = StartRecording
    | StopRecording
    | ClearRecording
    | SetButtonState ButtonState
    | Tick Posix
    | UpdateCurrentTime Posix


-- Update

calculateElapsedPixels : Posix -> Posix -> Float
calculateElapsedPixels startTime endTime =
    toFloat (Time.posixToMillis endTime - Time.posixToMillis startTime) / toFloat config.msToPixels


getEventsWidth : List Event -> Float
getEventsWidth events =
    sum (List.map .length events)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartRecording ->
            ( { initialModel | recording = True }, Task.perform UpdateCurrentTime now )

        StopRecording ->
            ( { model | recording = False, currentStart = Nothing }, Cmd.none )

        ClearRecording ->
            ( { model | events = [], currentState = Released }, Cmd.none )

        SetButtonState newState ->
            if model.recording && newState /= model.currentState then
                let
                    (updatedEvents, newCurrentStart) =
                        case (model.lastTime, model.currentStart) of
                            (Just lastTime, Just currentStart) ->
                                let
                                    elapsed = calculateElapsedPixels currentStart lastTime
                                    newEvent =
                                        { startX = getEventsWidth model.events
                                        , length = elapsed
                                        , state = model.currentState
                                        }
                                in
                                (newEvent :: model.events, Just lastTime)

                            _ ->
                                (model.events, model.currentStart)
                in
                ( { model | currentState = newState, events = updatedEvents, currentStart = newCurrentStart }, Cmd.none )
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

getTotalTimePixels : Model -> Float
getTotalTimePixels model =
    let
        eventsWidth = getEventsWidth model.events
        currentWidth =
            case (model.currentStart, model.lastTime) of
                (Just start, Just lastTime) ->
                    calculateElapsedPixels start lastTime
                _ ->
                    0
    in
    eventsWidth + currentWidth


getTotalTimeSeconds : Model -> Float
getTotalTimeSeconds model =
    getTotalTimePixels model / 10  -- 10 pixels = 1 second


getColorForState : ButtonState -> String
getColorForState state =
    case state of
        Pressed ->
            "blue"

        Released ->
            "green"


timeAxisMarkers : Float -> List (Svg Msg)
timeAxisMarkers totalPixels =
    let
        pixelsPerSecond = 10
        maxSeconds = ceiling (totalPixels / pixelsPerSecond)
        secondMarkers = List.range 0 maxSeconds
    in
    List.concatMap (\sec ->
        let
            xPos = toFloat sec * pixelsPerSecond
        in
        [ Svg.line
            [ SvgA.x1 (String.fromFloat xPos)
            , SvgA.y1 (String.fromInt config.timeAxisY)
            , SvgA.x2 (String.fromFloat xPos)
            , SvgA.y2 (String.fromInt (config.timeAxisY + config.timeAxisTickHeight))
            , SvgA.stroke "#666"
            , SvgA.strokeWidth "1"
            ] []
        , svgText
            [ SvgA.x (String.fromFloat xPos)
            , SvgA.y (String.fromInt config.timeAxisLabelY)
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
        totalPixels = getTotalTimePixels model
        totalSeconds = getTotalTimeSeconds model
        svgWidth = max config.minSvgWidth (totalPixels + config.svgPadding)

        recordingIndicator =
            if model.recording then
                div
                    [ style "display" "inline-block"
                    , style "margin-left" "10px"
                    , style "color" "red"
                    , style "font-weight" "bold"
                    , attribute "aria-live" "polite"
                    ]
                    [ text "⬤ RECORDING" ]
            else
                div
                    [ style "display" "inline-block"
                    , style "margin-left" "10px"
                    , style "color" "#999"
                    , attribute "aria-live" "polite"
                    ]
                    [ text "○ Not Recording" ]

        buttonColor =
            case model.currentState of
                Pressed -> "#2196F3"
                Released -> "#4CAF50"
    in
    div [ style "padding" "20px", style "font-family" "sans-serif" ]
        [ h1 [] [ text "Press Plotter" ]
        , p [] [ text "Visualize button press patterns over time" ]
        , div [ style "margin" "20px 0" ]
            [ button
                [ onClick StartRecording
                , style "margin-right" "10px"
                , style "padding" "10px 20px"
                , attribute "aria-label" "Start recording button presses"
                ]
                [ text "Start Recording" ]
            , button
                [ onClick StopRecording
                , style "margin-right" "10px"
                , style "padding" "10px 20px"
                , attribute "aria-label" "Stop recording button presses"
                ]
                [ text "Stop Recording" ]
            , button
                [ onClick ClearRecording
                , style "margin-right" "10px"
                , style "padding" "10px 20px"
                , style "background-color" "#ff9800"
                , style "color" "white"
                , style "border" "none"
                , attribute "aria-label" "Clear the recorded pattern"
                ]
                [ text "Clear" ]
            , button
                [ onMouseDown (SetButtonState Pressed)
                , onMouseUp (SetButtonState Released)
                , style "padding" "10px 20px"
                , style "background-color" buttonColor
                , style "color" "white"
                , style "border" "none"
                , style "cursor" "pointer"
                , style "transition" "background-color 0.15s ease"
                , attribute "aria-label" "Hold to record button press"
                , attribute "aria-pressed" (if model.currentState == Pressed then "true" else "false")
                ]
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
                [ text ("Duration: " ++ String.fromFloat totalSeconds ++ "s") ]
            ]
        , div [ style "overflow-x" "auto", style "margin" "20px 0" ]
            [ svg
                [ SvgA.width (String.fromFloat svgWidth)
                , SvgA.height (String.fromInt config.svgHeight)
                , SvgA.style "border: 1px solid #ccc; background-color: #f9f9f9;"
                , attribute "role" "img"
                , attribute "aria-label" ("Press pattern timeline showing " ++ String.fromFloat totalSeconds ++ " seconds of recording")
                ]
                (List.concatMap eventToRectangles model.events ++ [currentRectangle model] ++ timeAxisMarkers totalPixels)
            ]
        ]


eventToRectangles : Event -> List (Svg Msg)
eventToRectangles event =
    [ rect
        [ SvgA.x (String.fromFloat event.startX)
        , SvgA.y (String.fromInt config.timelineYStart)
        , SvgA.width (String.fromFloat event.length)
        , SvgA.height (String.fromInt config.timelineHeight)
        , SvgA.fill (getColorForState event.state)
        , SvgA.opacity "0.9"
        ]
        []
    ]


currentRectangle : Model -> Svg Msg
currentRectangle model =
    case (model.currentStart, model.lastTime) of
        (Just start, Just lastTime) ->
            let
                elapsed = calculateElapsedPixels start lastTime
                color = getColorForState model.currentState
            in
            rect
                [ SvgA.x (String.fromFloat (getEventsWidth model.events))
                , SvgA.y (String.fromInt config.timelineYStart)
                , SvgA.width (String.fromFloat elapsed)
                , SvgA.height (String.fromInt config.timelineHeight)
                , SvgA.fill color
                , SvgA.opacity "0.7"
                ]
                []

        _ ->
            rect [] []


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    every (toFloat config.tickIntervalMs) Tick


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
