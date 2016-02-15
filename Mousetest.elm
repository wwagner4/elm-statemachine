import Graphics.Element exposing (..)
import Mouse exposing (..)
import Signal exposing (..)
import Maybe exposing (..)
import Time exposing (..)
import Window exposing (..)


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


inpSig : Signal Inp
inpSig =
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
        timeSig = Time.every (Time.second )

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


main = Signal.map show inpSig
