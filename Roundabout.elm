import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text exposing (..)
import Time exposing (..)
import Maybe exposing (..)
import Signal exposing (..)
import Window exposing (..)


type alias State =
  { start : Time
  , duration : Time
  , id : StateId }

createState : Time -> Time -> StateId -> State
createState start duration id =
  { start = start
  , duration = duration
  , id = id }


type StateId = A | B | C

type StateOfState = Ready | Processing


stateOfState : State -> Time -> StateOfState
stateOfState state time =
  let
    relTime = time - state.start
  in
    if (relTime > state.duration) then Ready
    else Processing


updateReadySignal : State -> Time -> State
updateReadySignal state time =
  case state.id of
    A -> createState time (Time.second * 2) B
    B -> createState time (Time.second * 0.5) C
    C -> createState time Time.second A



updateState : Time -> Maybe State -> Maybe State
updateState time maybeState =
  let
    state = withDefault (initial time) maybeState
    sofs = stateOfState state time
    nextState = case sofs of
      Ready -> updateReadySignal state time
      Processing -> state
  in
    Just nextState

initial : Time -> State
initial time = createState time Time.second A


run : Signal (Maybe State)
run = Signal.foldp updateState Nothing (every (Time.second / 10))


viewState : State -> List Form
viewState state =
  let
    txt = case state.id of
      A -> fromString "-A-"
      B -> fromString "-B-"
      C -> fromString "-C-"
    form = txt
      |> Text.height 300
      |> monospace
      |> leftAligned
      |> toForm
  in
    [form]



view : (Int, Int) -> Maybe State -> Element
view (w, h) maybeState =
  let
    elems = case maybeState of
      Nothing -> []
      Just state -> viewState state
  in
    collage w h elems



main = Signal.map2 view dimensions run
