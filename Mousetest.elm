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


clickCount : Signal Int
clickCount =
  foldp (\click total -> total + 1) 0 Mouse.clicks


timeSig : Signal Time
timeSig = Time.every (Time.millisecond * 100)


inpSig : Signal Inp
inpSig =
  let
    mouseSig = Signal.sampleOn clicks Mouse.position
  in
    Signal.map3 inp mouseSig clickCount timeSig


main = Signal.map show inpSig
