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
import Mouse exposing (..)


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


modelA pos rot time =
  let
    seed = initialSeed (round time)
    behav = moveBehaviour MovementTypeA pos seed
  in
    model (A behav) pos rot time (Time.second * 4)

modelB pos rot time = model B pos rot time (Time.second * 1)
modelC pos rot time = model C pos rot time (Time.second * 1)

modelD pos rot time =
  let
    seed = initialSeed (round time)
    behav = moveBehaviour MovementTypeB pos seed
  in
    model (A behav) pos rot time (Time.second * 4)



type MovementType = MovementTypeA | MovementTypeB


type alias MoveBehaviour =
  { startPos : Pos
  , endPos : Pos
  , movementType : MovementType }

moveBehaviour : MovementType -> Pos -> Seed -> MoveBehaviour
moveBehaviour movementType pos seed =
  let
    maxVal = 600

    gen : Float -> Generator Float
    gen value =
      if value < 0 then Random.float (-maxVal * 0.3) maxVal
      else Random.float -maxVal (maxVal * 0.3)

    (dx, s1) = generate (gen pos.x) seed
    (dy, s2) = generate (gen pos.y) s1
  in
    { startPos = pos
    , endPos = { x = pos.x + dx, y = pos.y + dy }
    , movementType = movementType }


type State = A MoveBehaviour | B | C | D MoveBehaviour


type Transition = TransitionReady | TransitionProcessing


type alias Inp =
  { mousePos : (Float, Float)
  , onClickMousePos : Maybe (Float, Float)
  , clickCnt : Int
  , time : Time }


inp : (Float, Float) -> Maybe (Float, Float) -> Int -> Time -> Inp
inp (x, y) onClickMousePos clickCnt time =
  { mousePos = (x, y)
  , onClickMousePos = onClickMousePos
  , clickCnt = clickCnt
  , time = time }


updateModel : Inp -> Maybe Model -> Maybe Model
updateModel inp maybeModel =
  let
    transitionOf : Model -> Transition
    transitionOf model =
      if ((inp.time - model.startTime) > model.duration) then TransitionReady
      else TransitionProcessing


    updatePosOn : MoveBehaviour -> Model -> Pos
    updatePosOn moveBehaviour model =
      let
        relTime = inp.time - model.startTime
        x = ease easeOutBounce Easing.float moveBehaviour.startPos.x moveBehaviour.endPos.x model.duration relTime
        y = ease easeOutBounce Easing.float moveBehaviour.startPos.y moveBehaviour.endPos.y model.duration relTime
      in
        { x = x, y = y }


    model = withDefault (initial inp.time) maybeModel
    nextModel =
      case transitionOf model of
        TransitionReady ->
          case model.state of
            A moveBehaviour -> modelB model.pos model.rot inp.time
            B -> modelC model.pos model.rot inp.time
            C -> modelA model.pos model.rot inp.time
            D moveBehaviour -> modelB model.pos model.rot inp.time
        TransitionProcessing ->
          case inp.onClickMousePos of
            Nothing ->
              case model.state of
                A moveBehaviour -> { model | pos = (updatePosOn moveBehaviour model) }
                B -> model
                C -> { model | rot = model.rot + 0.1 }
                D moveBehaviour -> { model | pos = (updatePosOn moveBehaviour model) }
            Just _ ->
                modelD model.pos model.rot inp.time

  in
    Just nextModel


initial : Time -> Model
initial time = modelA (pos 0 0) 0 time


view1 : (Int, Int) -> Maybe Model -> Element
view1 (w, h) maybeModel = show maybeModel


view : (Int, Int) -> Maybe Model -> Element
view (w, h) maybeModel =
  let
    viewModel : Model -> List Form
    viewModel model =
      let
        size = 300

        bgForm : Color -> Form
        bgForm color = circle (size * 0.6)
          |> filled color

        txtForm : String -> Form
        txtForm txt = txt
          |> fromString
          |> Text.height size
          |> monospace
          |> centered
          |> toForm
          |> move (0, size * 0.045)

        grp = case model.state of
          A _ -> group [bgForm Color.red, txtForm "A"]
          B -> group [bgForm Color.green, txtForm "B"]
          C -> group [bgForm Color.yellow, txtForm "C"]
          D _ -> group [bgForm Color.lightBlue , txtForm "D"]
        grpTransformed = grp
          |> move (model.pos.x, model.pos.y)
          |> rotate model.rot
      in
        [grpTransformed]


    forms = case maybeModel of
      Nothing -> []
      Just model -> viewModel model
  in
    collage w h forms


inpSig : Time -> Signal Inp
inpSig timePeriode =
  let
    initialInp = inp (0, 0) Nothing 0 0


    continousInpSig : Signal Inp
    continousInpSig =
      let
        mouseSig : Signal (Int, Int)
        mouseSig = Signal.sampleOn clicks Mouse.position

        clickCountSig : Signal Int
        clickCountSig =
          foldp (\click total -> total + 1) 0 Mouse.clicks

        timeSig : Signal Time
        timeSig = Time.every timePeriode

        adjustPos : (Int, Int) -> (Int, Int) -> (Float, Float)
        adjustPos (x, y) (w, h) =
          let
            xoff = (toFloat w) / 2
            yoff = (toFloat h) / 2
          in
            ((toFloat x) - xoff, (toFloat y) - yoff)

        mouseSigAdjusted : Signal (Float, Float)
        mouseSigAdjusted = Signal.map2 adjustPos mouseSig dimensions


      in
        Signal.map4 inp mouseSigAdjusted (constant Nothing) clickCountSig timeSig


    nextInp : Inp -> Inp -> Inp
    nextInp currentInp previousInp =
      if currentInp.clickCnt > previousInp.clickCnt then { currentInp | onClickMousePos = Just currentInp.mousePos }
      else currentInp

  in
    foldp nextInp initialInp continousInpSig


modelSignal : Signal (Maybe Model)
modelSignal = Signal.foldp updateModel Nothing (inpSig (Time.millisecond * 10))


main = Signal.map2 view dimensions modelSignal
