import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Color exposing (..)
import List exposing (..)
import Random exposing (..)


type alias Model =
  { state : State
  , pos : Pos
  , startTime : Time
  , duration : Time }


model : State -> Pos -> Time -> Time -> Model
model state pos startTime duration =
  { state = state
  , pos = pos
  , startTime = startTime
  , duration = duration }


modelA pos time = model (A (astate pos time)) pos time (Time.second * 2)
modelB pos time = model B pos time (Time.second * 0.5)
modelC pos time = model C pos time (Time.second * 0.5)


type alias Pos =
  { x : Float
  , y : Float }


pos : Float -> Float -> Pos
pos x y = { x = x, y = y }

posZero : Pos
posZero = {x = 0, y = 0 }


updatePos : Pos -> Float -> Float -> Pos
updatePos pos dx dy =
  { x = pos.x + dx, y = pos.y + dy }


type alias AState =
  { startPos : Pos
  , seed : Seed }

astate : Pos -> Time -> AState
astate pos time =
  { startPos = pos
  , seed = initialSeed (round time) }


type State = A AState | B | C


type Transition = TransitionReady | TransitionProcessing


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel =
  let
    transitionOf : Model -> Transition
    transitionOf model =
      if ((time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updateOnReady : Model -> Model
    updateOnReady model = case model.state of
      A astate -> modelB model.pos time
      B -> modelC model.pos time
      C -> modelA model.pos time


    updateOnProcessing : Model -> Model
    updateOnProcessing model = case model.state of
      A astate -> { model | pos = updatePos model.pos 1 1 }
      B -> { model | pos = updatePos model.pos 0 -4 }
      C -> { model | pos = updatePos model.pos -4 0 }


    model = withDefault (initial time) maybeModel
    nextModel = case transitionOf model of
      TransitionReady -> updateOnReady model
      TransitionProcessing -> updateOnProcessing model
  in
    Just nextModel


initial : Time -> Model
initial time = modelA (pos 100 100) time


view1 : (Int, Int) -> Maybe Model -> Element
view1 (w, h) maybeModel = show maybeModel


view : (Int, Int) -> Maybe Model -> Element
view (w, h) maybeModel =
  let
    moveToPos : Form -> Pos -> Form
    moveToPos form pos = move (pos.x, pos.y) form


    viewModel : Model -> List Form
    viewModel model =
      let
        shape = square 250
        (txt, bgForm) = case model.state of
          A astate -> (fromString "A", filled Color.red shape)
          B -> (fromString "B", filled Color.green shape)
          C -> (fromString "C", filled Color.yellow shape)
        txtForm = txt
          |> Text.height 300
          |> monospace
          |> centered
          |> toForm
        bgMoved = moveToPos bgForm model.pos
        txtMoved = moveToPos txtForm model.pos
      in
        [bgMoved, txtMoved]


    forms = case maybeModel of
      Nothing -> []
      Just model -> viewModel model
  in
    collage w h forms


modelSignal : Signal (Maybe Model)
modelSignal = Signal.foldp updateModel Nothing (every (Time.millisecond * 10))


main = Signal.map2 view dimensions modelSignal
