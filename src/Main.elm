port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (attribute, disabled, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as SvgAttr
import Task
import Time


-- PORTS


port exportToConsole : Encode.Value -> Cmd msg


port scrollTimelineToEnd : () -> Cmd msg


port userScrolledLeft : (() -> msg) -> Sub msg


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL


type ButtonState
    = Pressed
    | Released


type alias Event =
    { startX : Float
    , length : Float
    , state : ButtonState
    }


type alias Statistics =
    { pressCount : Int
    , releaseCount : Int
    , totalPressTime : Float
    , totalReleaseTime : Float
    , averagePressTime : Float
    , averageReleaseTime : Float
    }


type alias Model =
    { events : List Event
    , recording : Bool
    , buttonState : ButtonState
    , currentEventStartTime : Float
    , elapsedTime : Float
    , pixelsPerSecond : Float
    , autoScroll : Bool
    , recordingStartTime : Int
    }


type Msg
    = StartRecording
    | StopRecording
    | ClearEvents
    | ButtonDown
    | ButtonUp
    | ButtonDownAt Time.Posix
    | ButtonUpAt Time.Posix
    | Tick Time.Posix
    | ExportData
    | ZoomIn
    | ZoomOut
    | ToggleAutoScroll
    | UserScrolledLeft


-- CONFIG


config =
    { tickInterval = 10 -- milliseconds (update interval for display - actual timing is timestamp-based)
    , defaultPixelsPerSecond = 50 -- default scale - can be adjusted with zoom controls
    , minPixelsPerSecond = 10 -- minimum zoom out
    , maxPixelsPerSecond = 800 -- maximum zoom in
    , timelineHeight = 100
    , eventHeight = 80
    , gridSpacing = 50
    , secondMarkerHeight = 20
    , svgHeight = 200
    , buttonPressedColor = "#3498db"
    , buttonReleasedColor = "#2ecc71"
    , pressEventColor = "#3498db"
    , releaseEventColor = "#2ecc71"
    , gridColor = "#e0e0e0"
    , secondMarkerColor = "#333"
    }


-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { events = []
      , recording = False
      , buttonState = Released
      , currentEventStartTime = 0
      , elapsedTime = 0
      , pixelsPerSecond = config.defaultPixelsPerSecond
      , autoScroll = True
      , recordingStartTime = 0
      }
    , Cmd.none
    )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartRecording ->
            ( { model
                | recording = True
                , events = []
                , elapsedTime = 0
                , currentEventStartTime = 0
                , buttonState = Released
                , recordingStartTime = 0
              }
            , Cmd.none
            )

        StopRecording ->
            let
                finalModel =
                    if model.recording then
                        finalizeCurrentEvent model

                    else
                        model
            in
            ( { finalModel | recording = False }, Cmd.none )

        ClearEvents ->
            ( { model
                | events = []
                , elapsedTime = 0
                , currentEventStartTime = 0
                , buttonState = Released
              }
            , Cmd.none
            )

        ButtonDown ->
            ( model, Task.perform ButtonDownAt Time.now )

        ButtonUp ->
            ( model, Task.perform ButtonUpAt Time.now )

        ButtonDownAt posix ->
            if model.recording && model.buttonState == Released then
                let
                    currentTimeMillis =
                        Time.posixToMillis posix

                    ( startTime, currentElapsed ) =
                        if model.recordingStartTime == 0 then
                            ( currentTimeMillis, 0 )

                        else
                            ( model.recordingStartTime
                            , toFloat (currentTimeMillis - model.recordingStartTime) / 1000
                            )

                    eventLength =
                        timeToPixels model.pixelsPerSecond (currentElapsed - model.currentEventStartTime)

                    newEvent =
                        { startX = timeToPixels model.pixelsPerSecond model.currentEventStartTime
                        , length = eventLength
                        , state = Released
                        }
                in
                ( { model
                    | buttonState = Pressed
                    , currentEventStartTime = currentElapsed
                    , events = model.events ++ [ newEvent ]
                    , elapsedTime = currentElapsed
                    , recordingStartTime = startTime
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ButtonUpAt posix ->
            if model.recording && model.buttonState == Pressed then
                let
                    currentTimeMillis =
                        Time.posixToMillis posix

                    ( startTime, currentElapsed ) =
                        if model.recordingStartTime == 0 then
                            ( currentTimeMillis, 0 )

                        else
                            ( model.recordingStartTime
                            , toFloat (currentTimeMillis - model.recordingStartTime) / 1000
                            )

                    eventLength =
                        timeToPixels model.pixelsPerSecond (currentElapsed - model.currentEventStartTime)

                    newEvent =
                        { startX = timeToPixels model.pixelsPerSecond model.currentEventStartTime
                        , length = eventLength
                        , state = Pressed
                        }
                in
                ( { model
                    | buttonState = Released
                    , currentEventStartTime = currentElapsed
                    , events = model.events ++ [ newEvent ]
                    , elapsedTime = currentElapsed
                    , recordingStartTime = startTime
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Tick posix ->
            if model.recording then
                let
                    currentTimeMillis =
                        Time.posixToMillis posix

                    ( startTime, elapsed ) =
                        if model.recordingStartTime == 0 then
                            -- First tick - set the start time
                            ( currentTimeMillis, 0 )

                        else
                            -- Calculate elapsed time from start
                            ( model.recordingStartTime
                            , toFloat (currentTimeMillis - model.recordingStartTime) / 1000
                            )

                    cmd =
                        if model.autoScroll then
                            scrollTimelineToEnd ()

                        else
                            Cmd.none
                in
                ( { model
                    | elapsedTime = elapsed
                    , recordingStartTime = startTime
                  }
                , cmd
                )

            else
                ( model, Cmd.none )

        ExportData ->
            ( model, exportToConsole (encodeModelData model) )

        ZoomIn ->
            let
                newScale =
                    min config.maxPixelsPerSecond (model.pixelsPerSecond * 2)
            in
            ( { model | pixelsPerSecond = newScale }, Cmd.none )

        ZoomOut ->
            let
                newScale =
                    max config.minPixelsPerSecond (model.pixelsPerSecond / 2)
            in
            ( { model | pixelsPerSecond = newScale }, Cmd.none )

        ToggleAutoScroll ->
            ( { model | autoScroll = not model.autoScroll }, Cmd.none )

        UserScrolledLeft ->
            ( { model | autoScroll = False }, Cmd.none )


finalizeCurrentEvent : Model -> Model
finalizeCurrentEvent model =
    let
        eventLength =
            timeToPixels model.pixelsPerSecond (model.elapsedTime - model.currentEventStartTime)

        newEvent =
            { startX = timeToPixels model.pixelsPerSecond model.currentEventStartTime
            , length = eventLength
            , state = model.buttonState
            }
    in
    { model | events = model.events ++ [ newEvent ] }


-- HELPER FUNCTIONS


timeToPixels : Float -> Float -> Float
timeToPixels pixelsPerSecond seconds =
    seconds * pixelsPerSecond


pixelsToTime : Float -> Float -> Float
pixelsToTime pixelsPerSecond pixels =
    pixels / pixelsPerSecond


calculateStatistics : Model -> Statistics
calculateStatistics model =
    let
        allEvents =
            if model.recording then
                finalizeCurrentEvent model |> .events

            else
                model.events

        pressEvents =
            List.filter (\e -> e.state == Pressed) allEvents

        releaseEvents =
            List.filter (\e -> e.state == Released && e.startX > 0) allEvents

        totalPressTime =
            pressEvents
                |> List.map .length
                |> List.sum
                |> pixelsToTime model.pixelsPerSecond

        totalReleaseTime =
            releaseEvents
                |> List.map .length
                |> List.sum
                |> pixelsToTime model.pixelsPerSecond

        pressCount =
            List.length pressEvents

        releaseCount =
            List.length releaseEvents

        averagePressTime =
            if pressCount > 0 then
                totalPressTime / toFloat pressCount

            else
                0

        averageReleaseTime =
            if releaseCount > 0 then
                totalReleaseTime / toFloat releaseCount

            else
                0
    in
    { pressCount = pressCount
    , releaseCount = releaseCount
    , totalPressTime = totalPressTime
    , totalReleaseTime = totalReleaseTime
    , averagePressTime = averagePressTime
    , averageReleaseTime = averageReleaseTime
    }


formatTime : Float -> String
formatTime seconds =
    let
        mins =
            floor seconds // 60

        secs =
            modBy 60 (floor seconds)

        ms =
            floor ((seconds - toFloat (floor seconds)) * 100)
    in
    String.padLeft 2 '0' (String.fromInt mins)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt secs)
        ++ "."
        ++ String.padLeft 2 '0' (String.fromInt ms)


encodeModelData : Model -> Encode.Value
encodeModelData model =
    let
        allEvents =
            if model.recording then
                finalizeCurrentEvent model |> .events

            else
                model.events

        encodeButtonState state =
            case state of
                Pressed ->
                    Encode.string "pressed"

                Released ->
                    Encode.string "released"

        encodeEvent event =
            Encode.object
                [ ( "startTime", Encode.float (pixelsToTime model.pixelsPerSecond event.startX) )
                , ( "duration", Encode.float (pixelsToTime model.pixelsPerSecond event.length) )
                , ( "state", encodeButtonState event.state )
                ]
    in
    Encode.object
        [ ( "totalDuration", Encode.float model.elapsedTime )
        , ( "events", Encode.list encodeEvent allEvents )
        , ( "statistics", encodeStatistics (calculateStatistics model) )
        ]


encodeStatistics : Statistics -> Encode.Value
encodeStatistics stats =
    Encode.object
        [ ( "pressCount", Encode.int stats.pressCount )
        , ( "releaseCount", Encode.int stats.releaseCount )
        , ( "totalPressTime", Encode.float stats.totalPressTime )
        , ( "totalReleaseTime", Encode.float stats.totalReleaseTime )
        , ( "averagePressTime", Encode.float stats.averagePressTime )
        , ( "averageReleaseTime", Encode.float stats.averageReleaseTime )
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.recording then
            Time.every (toFloat config.tickInterval) Tick

          else
            Sub.none
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyUp keyUpDecoder
        , userScrolledLeft (\_ -> UserScrolledLeft)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == " " then
                    Decode.succeed ButtonDown

                else
                    Decode.fail "Not space"
            )


keyUpDecoder : Decode.Decoder Msg
keyUpDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == " " then
                    Decode.succeed ButtonUp

                else
                    Decode.fail "Not space"
            )


-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Arial, sans-serif"
        , style "padding" "20px"
        , style "max-width" "100%"
        ]
        [ h1 [] [ Html.text "Press Plotter" ]
        , viewControls model
        , viewStatistics model
        , viewTimeline model
        , viewRecordButton model
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ style "margin-bottom" "20px" ]
        [ if not model.recording then
            button
                [ onClick StartRecording
                , style "padding" "10px 20px"
                , style "margin-right" "10px"
                , style "font-size" "16px"
                , style "cursor" "pointer"
                , attribute "aria-label" "Start recording button presses"
                ]
                [ Html.text "Start Recording" ]

          else
            button
                [ onClick StopRecording
                , style "padding" "10px 20px"
                , style "margin-right" "10px"
                , style "font-size" "16px"
                , style "cursor" "pointer"
                , style "background-color" "#e74c3c"
                , style "color" "white"
                , style "border" "none"
                , attribute "aria-label" "Stop recording button presses"
                ]
                [ Html.text "Stop Recording" ]
        , button
            [ onClick ClearEvents
            , style "padding" "10px 20px"
            , style "margin-right" "10px"
            , style "font-size" "16px"
            , style "cursor" "pointer"
            , disabled (not model.recording && List.isEmpty model.events)
            , attribute "aria-label" "Clear all recorded events"
            ]
            [ Html.text "Clear" ]
        , button
            [ onClick ExportData
            , style "padding" "10px 20px"
            , style "margin-right" "10px"
            , style "font-size" "16px"
            , style "cursor" "pointer"
            , disabled (List.isEmpty model.events && not model.recording)
            , attribute "aria-label" "Export recording data to console"
            ]
            [ Html.text "Export Data" ]
        , span
            [ style "margin-left" "10px"
            , style "margin-right" "10px"
            , style "color" "#666"
            ]
            [ Html.text "Zoom:" ]
        , button
            [ onClick ZoomOut
            , style "padding" "10px 15px"
            , style "margin-right" "5px"
            , style "font-size" "16px"
            , style "cursor" "pointer"
            , disabled (model.pixelsPerSecond <= config.minPixelsPerSecond)
            , attribute "aria-label" "Zoom out timeline"
            ]
            [ Html.text "-" ]
        , span
            [ style "margin-right" "5px"
            , style "font-size" "14px"
            , style "color" "#666"
            ]
            [ Html.text (String.fromInt (round model.pixelsPerSecond) ++ "px/s") ]
        , button
            [ onClick ZoomIn
            , style "padding" "10px 15px"
            , style "margin-right" "20px"
            , style "font-size" "16px"
            , style "cursor" "pointer"
            , disabled (model.pixelsPerSecond >= config.maxPixelsPerSecond)
            , attribute "aria-label" "Zoom in timeline"
            ]
            [ Html.text "+" ]
        , button
            [ onClick ToggleAutoScroll
            , style "padding" "10px 15px"
            , style "font-size" "14px"
            , style "cursor" "pointer"
            , style "background-color" (if model.autoScroll then "#3498db" else "#95a5a6")
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "4px"
            , attribute "aria-label" "Toggle auto-scroll"
            ]
            [ Html.text (if model.autoScroll then "Auto-scroll: ON" else "Auto-scroll: OFF") ]
        , if model.recording then
            span
                [ style "margin-left" "20px"
                , style "padding" "5px 12px"
                , style "background-color" "#fee"
                , style "color" "#c0392b"
                , style "font-weight" "bold"
                , style "border-radius" "4px"
                , style "border" "1px solid #e74c3c"
                , style "font-size" "14px"
                , attribute "role" "status"
                , attribute "aria-live" "polite"
                ]
                [ Html.text "REC" ]

          else
            Html.text ""
        ]


viewStatistics : Model -> Html Msg
viewStatistics model =
    let
        stats =
            calculateStatistics model

        fractionPressed =
            if model.elapsedTime > 0 then
                (stats.totalPressTime / model.elapsedTime) * 100

            else
                0
    in
    div
        [ style "margin-bottom" "20px"
        , style "padding" "15px"
        , style "background-color" "#f5f5f5"
        , style "border-radius" "5px"
        , attribute "role" "region"
        , attribute "aria-label" "Recording statistics"
        ]
        [ h3 [ style "margin-top" "0" ] [ Html.text "Statistics" ]
        , div [ style "display" "grid", style "grid-template-columns" "repeat(3, 1fr)", style "gap" "10px" ]
            [ viewStatItem "Press Count" (String.fromInt stats.pressCount)
            , viewStatItem "Release Count" (String.fromInt stats.releaseCount)
            , viewStatItem "Total Duration" (formatTime model.elapsedTime)
            , viewStatItem "Total Press Time" (formatTime stats.totalPressTime)
            , viewStatItem "Total Release Time" (formatTime stats.totalReleaseTime)
            , viewStatItem "Fraction Pressed" (String.fromFloat (toFloat (round (fractionPressed * 10)) / 10) ++ "%")
            , viewStatItem "Avg Press Duration" (formatTime stats.averagePressTime)
            , viewStatItem "Avg Release Duration" (formatTime stats.averageReleaseTime)
            , Html.text ""
            ]
        ]


viewStatItem : String -> String -> Html Msg
viewStatItem label value =
    div []
        [ div [ style "font-size" "12px", style "color" "#666" ] [ Html.text label ]
        , div [ style "font-size" "18px", style "font-weight" "bold" ] [ Html.text value ]
        ]


viewTimeline : Model -> Html Msg
viewTimeline model =
    let
        currentWidth =
            timeToPixels model.pixelsPerSecond model.elapsedTime

        totalWidth =
            max 1000 (currentWidth + 100)

        currentEvent =
            if model.recording then
                let
                    eventLength =
                        timeToPixels model.pixelsPerSecond (model.elapsedTime - model.currentEventStartTime)
                in
                [ { startX = timeToPixels model.pixelsPerSecond model.currentEventStartTime
                  , length = eventLength
                  , state = model.buttonState
                  }
                ]

            else
                []

        allEvents =
            model.events ++ currentEvent
    in
    div
        [ attribute "id" "timeline-container"
        , style "margin-bottom" "20px"
        , style "overflow-x" "auto"
        , style "border" "1px solid #ccc"
        , style "background-color" "white"
        , attribute "role" "img"
        , attribute "aria-label" "Timeline visualization of button presses"
        ]
        [ svg
            [ SvgAttr.width (String.fromFloat totalWidth)
            , SvgAttr.height (String.fromInt config.svgHeight)
            , SvgAttr.style "display: block;"
            ]
            (viewGrid totalWidth ++ viewSecondMarkers model.pixelsPerSecond totalWidth ++ viewEvents allEvents)
        , div
            [ style "padding" "5px 10px"
            , style "background-color" "#f9f9f9"
            , style "border-top" "1px solid #ccc"
            , style "font-size" "14px"
            ]
            [ Html.text ("Duration: " ++ formatTime model.elapsedTime) ]
        ]


viewGrid : Float -> List (Svg Msg)
viewGrid totalWidth =
    let
        numLines =
            ceiling (totalWidth / toFloat config.gridSpacing)

        verticalLines =
            List.range 0 numLines
                |> List.map
                    (\i ->
                        line
                            [ SvgAttr.x1 (String.fromInt (i * config.gridSpacing))
                            , SvgAttr.y1 "0"
                            , SvgAttr.x2 (String.fromInt (i * config.gridSpacing))
                            , SvgAttr.y2 (String.fromInt config.svgHeight)
                            , SvgAttr.stroke config.gridColor
                            , SvgAttr.strokeWidth "1"
                            ]
                            []
                    )

        horizontalLines =
            List.range 0 (config.svgHeight // config.gridSpacing)
                |> List.map
                    (\i ->
                        line
                            [ SvgAttr.x1 "0"
                            , SvgAttr.y1 (String.fromInt (i * config.gridSpacing))
                            , SvgAttr.x2 (String.fromFloat totalWidth)
                            , SvgAttr.y2 (String.fromInt (i * config.gridSpacing))
                            , SvgAttr.stroke config.gridColor
                            , SvgAttr.strokeWidth "1"
                            ]
                            []
                    )
    in
    verticalLines ++ horizontalLines


viewSecondMarkers : Float -> Float -> List (Svg Msg)
viewSecondMarkers pixelsPerSecond totalWidth =
    let
        numSeconds =
            ceiling (totalWidth / pixelsPerSecond)

        pps =
            round pixelsPerSecond
    in
    List.range 0 numSeconds
        |> List.map
            (\i ->
                g []
                    [ line
                        [ SvgAttr.x1 (String.fromInt (i * pps))
                        , SvgAttr.y1 "0"
                        , SvgAttr.x2 (String.fromInt (i * pps))
                        , SvgAttr.y2 (String.fromInt config.secondMarkerHeight)
                        , SvgAttr.stroke config.secondMarkerColor
                        , SvgAttr.strokeWidth "2"
                        ]
                        []
                    , text_
                        [ SvgAttr.x (String.fromInt (i * pps + 5))
                        , SvgAttr.y "15"
                        , SvgAttr.fontSize "12"
                        , SvgAttr.fill config.secondMarkerColor
                        ]
                        [ Svg.text (String.fromInt i ++ "s") ]
                    ]
            )


viewEvents : List Event -> List (Svg Msg)
viewEvents events =
    events
        |> List.map
            (\event ->
                rect
                    [ SvgAttr.x (String.fromFloat event.startX)
                    , SvgAttr.y (String.fromInt ((config.svgHeight - config.eventHeight) // 2))
                    , SvgAttr.width (String.fromFloat event.length)
                    , SvgAttr.height (String.fromInt config.eventHeight)
                    , SvgAttr.fill
                        (case event.state of
                            Pressed ->
                                config.pressEventColor

                            Released ->
                                config.releaseEventColor
                        )
                    , SvgAttr.opacity "0.7"
                    ]
                    []
            )


viewRecordButton : Model -> Html Msg
viewRecordButton model =
    div []
        [ button
            [ Html.Events.custom "mousedown"
                (Decode.succeed
                    { message = ButtonDown
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
            , Html.Events.custom "mouseup"
                (Decode.succeed
                    { message = ButtonUp
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
            , Html.Events.custom "touchstart"
                (Decode.succeed
                    { message = ButtonDown
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
            , Html.Events.custom "touchend"
                (Decode.succeed
                    { message = ButtonUp
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
            , style "padding" "20px 40px"
            , style "font-size" "18px"
            , style "cursor" "pointer"
            , style "background-color"
                (case model.buttonState of
                    Pressed ->
                        config.buttonPressedColor

                    Released ->
                        config.buttonReleasedColor
                )
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "5px"
            , style "user-select" "none"
            , style "-webkit-user-select" "none"
            , style "-webkit-touch-callout" "none"
            , style "touch-action" "manipulation"
            , disabled (not model.recording)
            , attribute "aria-label" "Hold to record press, release to record release"
            , attribute "aria-pressed"
                (case model.buttonState of
                    Pressed ->
                        "true"

                    Released ->
                        "false"
                )
            ]
            [ Html.text "Hold Me to Record Press" ]
        , div
            [ style "margin-top" "10px"
            , style "font-size" "14px"
            , style "color" "#666"
            ]
            [ Html.text "Tip: You can also use the spacebar to press/release" ]
        ]
