import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Color exposing (..)


type alias Model =
  { state : State
  , startTime : Time
  , duration : Time }


model : State -> Time -> Time -> Model
model state startTime duration =
  { state = state
  , startTime = startTime
  , duration = duration }


modelA time = model A time (Time.second * 2)
modelB time = model B time (Time.second * 0.5)
modelC time = model C time (Time.second * 0.5)


type alias Pos =
  { x : Float
  , y : Float }


pos : Float -> Float -> Pos
pos x y = { x = x, y = y}


type State = A | B | C


type Transition = TransitionReady | TransitionProcessing


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel =
  let
    transition : Model -> Transition
    transition model =
      if ((time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updateOnReady : Model -> Model
    updateOnReady model =
      case model.state of
        A -> modelB time
        B -> modelC time
        C -> modelA time


    model = withDefault (initial time) maybeModel
    nextModel = case transition model of
      TransitionReady -> updateOnReady model
      TransitionProcessing -> model
  in
    Just nextModel


initial : Time -> Model
initial time = modelA time


view : (Int, Int) -> Maybe Model -> Element
view (w, h) maybeModel =
  let
    viewState : Model -> List Form
    viewState model =
      let
        shape = square 250
        (txt, bgForm) = case model.state of
          A -> (fromString "A", filled Color.red shape)
          B -> (fromString "B", filled Color.green shape)
          C -> (fromString "C", filled Color.yellow shape)
        txtForm = txt
          |> Text.height 300
          |> monospace
          |> centered
          |> toForm
      in
        [bgForm, txtForm]



    elems = case maybeModel of
      Nothing -> []
      Just state -> viewState state
  in
    collage w h elems


modelSignal : Signal (Maybe Model)
modelSignal = Signal.foldp updateModel Nothing (every (Time.millisecond))


main = Signal.map2 view dimensions modelSignal
