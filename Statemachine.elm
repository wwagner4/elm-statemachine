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


type alias Pos =
  { x : Float
  , y : Float }


pos : Float -> Float -> Pos
pos x y = { x = x, y = y }

posZero : Pos
posZero = {x = 0, y = 0 }


type alias Model =
  { state : State
  , pos : Pos
  , rot : Float
  , startTime : Time
  , duration : Time }


model : State -> Pos -> Float -> Time -> Time -> Model
model state pos rot startTime duration =
  { state = state
  , pos = pos
  , rot = rot
  , startTime = startTime
  , duration = duration }


modelA pos rot time = model (A (moveBehaviour pos time)) pos rot time (Time.second * 4)
modelB pos rot time = model B pos rot time (Time.second * 1)
modelC pos rot time = model C pos rot time (Time.second * 1)


updatePos : Pos -> Float -> Float -> Pos
updatePos pos dx dy =
  { x = pos.x + dx, y = pos.y + dy }


type alias MoveBehaviour =
  { startPos : Pos
  , endPos : Pos }

moveBehaviour : Pos -> Time -> MoveBehaviour
moveBehaviour pos time =
  let
    maxVal = 600

    gen : Float -> Generator Float
    gen value =
      if value < 0 then Random.float (-maxVal * 0.3) maxVal
      else Random.float -maxVal (maxVal * 0.3)

    s0 = initialSeed (round time)
    (dx, s1) = generate (gen pos.x) s0
    (dy, s2) = generate (gen pos.y) s1
  in
    { startPos = pos
    , endPos = { x = pos.x + dx, y = pos.y + dy } }


type State = A MoveBehaviour | B | C


type Transition = TransitionReady | TransitionProcessing


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel =
  let
    transitionOf : Model -> Transition
    transitionOf model =
      if ((time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updatePosOn : MoveBehaviour -> Model -> Pos
    updatePosOn moveBehaviour model =
      let
        relTime = time - model.startTime
        x = ease easeOutBounce Easing.float moveBehaviour.startPos.x moveBehaviour.endPos.x model.duration relTime
        y = ease easeOutBounce Easing.float moveBehaviour.startPos.y moveBehaviour.endPos.y model.duration relTime
      in
        { x = x, y = y }


    model = withDefault (initial time) maybeModel
    nextModel =
      case transitionOf model of
        TransitionReady ->
          case model.state of
            A moveBehaviour -> modelB model.pos model.rot time
            B -> modelC model.pos model.rot time
            C -> modelA model.pos model.rot time
        TransitionProcessing ->
          case model.state of
            A moveBehaviour -> { model | pos = (updatePosOn moveBehaviour model) }
            B -> model
            C -> { model | rot = model.rot + 0.1 }

  in
    Just nextModel


initial : Time -> Model
initial time = modelA (pos 0 0) 0 time


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
          A moveBehaviour -> (fromString "A", filled Color.red shape)
          B -> (fromString "B", filled Color.green shape)
          C -> (fromString "C", filled Color.yellow shape)
        txtForm = txt
          |> Text.height 300
          |> monospace
          |> centered
          |> toForm
        bgMoved = moveToPos bgForm model.pos
        txtMoved = moveToPos txtForm model.pos
        forms = [bgMoved, txtMoved]
      in
        List.map (rotate (model.rot)) forms


    forms = case maybeModel of
      Nothing -> []
      Just model -> viewModel model
  in
    collage w h forms


modelSignal : Signal (Maybe Model)
modelSignal = Signal.foldp updateModel Nothing (every (Time.millisecond * 30))


main = Signal.map2 view dimensions modelSignal
