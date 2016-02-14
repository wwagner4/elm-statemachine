import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Color exposing (..)


type alias State =
  { spec : StateSpec
  , start : Time
  , duration : Time }


newState : StateSpec -> Time -> Time -> State
newState spec start duration =
  { spec = spec
  , start = start
  , duration = duration }


type StateSpec = A | B | C

type Sig = SigReady | SigProcessing


updateState : Time -> Maybe State -> Maybe State
updateState time maybeState =
  let
    sig : State -> Sig
    sig state =
      if ((time - state.start) > state.duration) then SigReady
      else SigProcessing


    nextOnReady : State -> State
    nextOnReady state =
      case state.spec of
        A -> newState B time (Time.second * 2)
        B -> newState C time (Time.second * 0.5)
        C -> newState A time (Time.second * 0.2)


    state = withDefault (initial time) maybeState
    nextState = case sig state of
      SigReady -> nextOnReady state
      SigProcessing -> state
  in
    Just nextState


initial : Time -> State
initial time = newState A time Time.second


view : (Int, Int) -> Maybe State -> Element
view (w, h) maybeState =
  let
    viewState : State -> List Form
    viewState state =
      let
        height = 300
        bgForm color = square height
          |> filled color
          |> move (0, -height / 30)
        (txt, bgColorForm) = case state.spec of
          A -> (fromString "A", bgForm Color.lightOrange )
          B -> (fromString "B", bgForm Color.lightGreen )
          C -> (fromString "C", bgForm Color.lightYellow )
        txtForm = txt
          |> Text.height height
          |> monospace
          |> centered
          |> toForm
      in
        [bgColorForm, txtForm]



    elems = case maybeState of
      Nothing -> []
      Just state -> viewState state
  in
    collage w h elems


stateSignal : Signal (Maybe State)
stateSignal = Signal.foldp updateState Nothing (every (Time.second / 10))


main = Signal.map2 view dimensions stateSignal
