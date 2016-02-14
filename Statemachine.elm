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
import Easing exposing (..)


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


modelA pos time = model (A (stateOfA pos time)) pos time (Time.second * 4)
modelB pos time = model B pos time (Time.second * 1)
modelC pos time = model C pos time (Time.second * 1)


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


type alias StateOfA =
  { startPos : Pos
  , endPos : Pos }

stateOfA : Pos -> Time -> StateOfA
stateOfA pos time =
  let
    s0 = initialSeed (round time)
    maxVal = 400
    (dx, s1) = generate (Random.float -maxVal maxVal) s0
    (dy, s2) = generate (Random.float -maxVal maxVal) s1
  in
    { startPos = pos
    , endPos = { x = pos.x + dx, y = pos.y + dy } }


type State = A StateOfA | B | C


type Transition = TransitionReady | TransitionProcessing


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel =
  let
    transitionOf : Model -> Transition
    transitionOf model =
      if ((time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updateOnReady : Model -> Model
    updateOnReady model =
      case model.state of
        A stateOfA -> modelB model.pos time
        B -> modelC model.pos time
        C -> modelA model.pos time


    updateOnProcessing : Model -> Model
    updateOnProcessing model =
      let
        updatePosOnA : StateOfA -> Model -> Pos
        updatePosOnA stateOfA model =
          let
            relTime = time - model.startTime
            x = ease easeOutCubic Easing.float stateOfA.startPos.x stateOfA.endPos.x model.duration relTime
            y = ease easeOutCubic Easing.float stateOfA.startPos.y stateOfA.endPos.y model.duration relTime
          in
            { x = x, y = y }
      in
        case model.state of
          A stateOfA -> { model | pos = (updatePosOnA stateOfA model) }
          B -> model
          C -> model


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
          A stateOfA -> (fromString "A", filled Color.red shape)
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
