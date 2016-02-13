import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Color exposing (..)
import List exposing (..)


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


modelA pos time = model A pos time (Time.second * 2)
modelB pos time = model B pos time (Time.second * 0.5)
modelC pos time = model C pos time (Time.second * 0.5)


type alias Pos =
  { x : Float
  , y : Float }


pos : Float -> Float -> Pos
pos x y = { x = x, y = y }

posZero : Pos
posZero = {x  = 0, y = 0 }


type State = A | B | C


type Transition = TransitionReady | TransitionProcessing


updateModel : Time -> Maybe Model -> Maybe Model
updateModel time maybeModel =
  let
    updatePos : Pos -> Float -> Float -> Pos
    updatePos pos dx dy =
      { x = pos.x + dx, y = pos.y + dy }


    transition : Model -> Transition
    transition model =
      if ((time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updateOnReady : Model -> Model
    updateOnReady model = case model.state of
      A -> modelB model.pos time
      B -> modelC model.pos time
      C -> modelA model.pos time


    updateOnProcessing : Model -> Model
    updateOnProcessing model = model

    model = withDefault (initial time) maybeModel
    nextModel = case transition model of
      TransitionReady -> updateOnReady model
      TransitionProcessing -> updateOnProcessing model
  in
    Just nextModel


initial : Time -> Model
initial time = modelA posZero time


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
          A -> (fromString "A", filled Color.red shape)
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


    elems = case maybeModel of
      Nothing -> []
      Just model -> viewModel model
  in
    collage w h elems


modelSignal : Signal (Maybe Model)
modelSignal = Signal.foldp updateModel Nothing (every (Time.millisecond))


main = Signal.map2 view1 dimensions modelSignal
