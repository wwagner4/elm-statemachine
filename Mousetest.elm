import Graphics.Element exposing (..)
import Mouse exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Time exposing (..)
import Window exposing (..)


type alias Inp =
  { mousePos : (Float, Float)
  , clickCnt : Int
  , time : Time }


inp : (Float, Float) -> Int -> Time -> Inp
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
    Signal.map3 inp mouseSigAdjusted clickCountSig timeSig


main = Signal.map show inpSig
