module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect)
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

        SetValue value ->
            if model.recording then
                let
                    (updatedEvents, newCurrentStart) =
                        case (model.lastTime, model.currentStart) of
                            (Just lastTime, Just currentStart) ->
                                let
                                    elapsed = (toFloat (Time.posixToMillis lastTime - Time.posixToMillis currentStart)) / 500
                                    newEvent = { startX = sum (List.map .length model.events), length = elapsed, color = if value == 1 then "blue" else "green" }
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


-- View

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick StartRecording ] [ text "Start" ]
        , button [ onClick StopRecording ] [ text "Stop" ]
        , button [ onClick (SetValue 1) ] [ text "Press Me" ]
        , svg [ SvgA.width "800", SvgA.height "400" ]
            (List.concatMap eventToRectangles model.events ++ [currentRectangle model])
        ]


eventToRectangles : Event -> List (Svg Msg)
eventToRectangles event =
    [ rect [ SvgA.x (String.fromFloat event.startX), SvgA.y "50", SvgA.width (String.fromFloat event.length), SvgA.height "300", SvgA.fill event.color ] [] ]


currentRectangle : Model -> Svg Msg
currentRectangle model =
    case (model.currentStart, model.lastTime) of
        (Just start, Just lastTime) ->
            let
                elapsed = (toFloat (Time.posixToMillis lastTime - Time.posixToMillis start)) / 500
                color = if model.currentValue == 1 then "blue" else "green"
            in
            rect [ SvgA.x (String.fromFloat (sum (List.map .length model.events))), SvgA.y "50", SvgA.width (String.fromFloat elapsed), SvgA.height "300", SvgA.fill color ] []

        _ ->
            rect [] []


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    every 500 Tick


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
