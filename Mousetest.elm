import Graphics.Element exposing (..)
import Mouse exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Time exposing (..)


type alias Inp =
  { mousePos : (Int, Int)
  , clickCnt : Int
  , time : Time }


inp : (Int, Int) -> Int -> Time -> Inp
inp (x, y) clickCnt time =
  { mousePos = (x, y)
  , clickCnt = clickCnt
  , time = time }


inpSig : Signal Inp
inpSig =
  let
    mouseSig : Signal (Int, Int)
    mouseSig = Signal.sampleOn clicks Mouse.position

    clickCountSig : Signal Int
    clickCountSig =
      foldp (\click total -> total + 1) 0 Mouse.clicks

    timeSig : Signal Time
    timeSig = Time.every (Time.millisecond * 100)

  in
    Signal.map3 inp mouseSig clickCountSig timeSig


main = Signal.map show inpSig
