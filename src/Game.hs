module Game where 

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort


data Field = Field
  { alive :: AliveCells
  , width :: Int
  , height :: Int
  }

cellSize :: Float 
cellSize = 30

initField :: Field
initField = Field [(0, 0), (1, 1), (4, 2)] 10 10

type Cell = (Int, Int)

type AliveCells = [Cell]

viewPort :: ViewPort
viewPort = ViewPort (negate . (/ 2) . (subtract cellSize) . fst $ cellToScreen (width initField, height initField),
  negate . (/ 2) . (subtract cellSize) . snd $ cellToScreen (width initField, height initField)) 0 1

renderer :: Field -> Picture
renderer field = applyViewPortToPicture viewPort 
  (pictures ([uncurry translate (cellToScreen (x, y)) (color black (rectangleWire cellSize cellSize))
    | x <- [0 .. width field - 1], y <- [0 .. height field - 1]] ++ [uncurry translate (cellToScreen cell) (color black
    (rectangleSolid cellSize cellSize)) | cell <- alive field]))

cellToScreen :: Cell -> (Float, Float)
cellToScreen (x, y) = (fromIntegral x * cellSize, fromIntegral y * cellSize)

oneIter :: Float -> Field -> Field
oneIter _ field = field

handler :: Event -> Field -> Field
handler _ field = field

startGame :: Field -> IO ()
startGame field = play window background fps field renderer handler oneIter
  where
    window = InWindow "Good Window" (500, 500) (200, 200)
    background = white
    fps = 30