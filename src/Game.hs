module Game where 

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Data.List


-- | Состояние игрового поля
data Field = Field
  { alive :: [Cell] -- ^ живые клетки
  , width :: Int    -- ^ ширина игрового поля
  , height :: Int   -- ^ высота игрового поля
  , pause :: Bool   -- ^ флаг паузы игры
  } deriving (Show)

-- | Размер каждой клетки поля при отрисовке
cellSize :: Float 
cellSize = 25

-- | Клетка поля
type Cell = (Int, Int)

-- | Проекция, для того чтобы поле отображалось по центру окна
viewPort :: Field -> ViewPort
viewPort field = ViewPort (negate . (/ 2) . (subtract cellSize) . fst $ cellToScreen (width field, height field),
  negate . (/ 2) . (subtract cellSize) . snd $ cellToScreen (width field, height field)) 0 1

-- | Отобразить игровое поле
renderer :: Field -> Picture
renderer field
 | alive field == [] = (applyViewPortToPicture . viewPort) field (pictures 
  ([showDead (x, y) | x <- [0 .. width field - 1], y <- [0 .. height field - 1]]
    ++ [showAlive cell | cell <- alive field] ++ [showPause (pause field), 
    uncurry translate (cellToScreen (0, 8)) (scale 0.14 0.14 (text "Left click to change state of the cell")),    
    uncurry translate (cellToScreen (0, 7)) (scale 0.14 0.14 (text "Space key to play or pause the game")),
    uncurry translate (cellToScreen (0, 6)) (scale 0.14 0.14 (text "Up and Down keys to change height")),
    uncurry translate (cellToScreen (0, 5)) (scale 0.14 0.14 (text "Left and Right keys to change width")),
    uncurry translate (cellToScreen (0, 4)) (scale 0.14 0.14 (text "C to clean all cells")),
    uncurry translate (cellToScreen (0, 3)) (scale 0.14 0.14 (text "Escape to quit"))]))
 | otherwise = (applyViewPortToPicture . viewPort) field (pictures 
  ([showDead (x, y) | x <- [0 .. width field - 1], y <- [0 .. height field - 1]]
    ++ [showAlive cell | cell <- alive field] ++ [showPause (pause field)]))

-- | Отобразить мертвую клетку
showDead :: Cell -> Picture
showDead cell = uncurry translate (cellToScreen cell) (color black (rectangleWire cellSize cellSize))

-- | Отобразить живую клетку
showAlive :: Cell -> Picture
showAlive cell = uncurry translate (cellToScreen cell) (color black (rectangleSolid cellSize cellSize))

-- | Отобразить, стоит ли игра на паузе
showPause :: Bool -> Picture
showPause True = scale 0.5 0.5 (color red (text "Pause"))
showPause False = Blank

-- | Преобразовать координаты на игровом поле в координаты на изображении
cellToScreen :: Cell -> Point
cellToScreen (x, y) = (fromIntegral x * cellSize, fromIntegral y * cellSize)

-- | Совершает один шаг игры 
oneStep :: Float -> Field -> Field
oneStep _ field
  | pause field == True = field
  | otherwise = applyRules field

-- | Применяет правила игры к полю
applyRules :: Field -> Field
applyRules field = Field ([(x, y) | x <- [0 .. width field - 1], y <- [0 .. height field - 1], 
  ((x, y) `elem` alive field && (length (neighbours (x, y) field) == 2 || length (neighbours (x, y) field) == 3)) ||
  (not ((x, y) `elem` alive field) && (length (neighbours (x, y) field) == 3))]) (width field) (height field) (pause field)

-- | Поиск живых соседей клетки
neighbours :: Cell -> Field -> [Cell]
neighbours (x, y) field = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (x + dx, y + dy) `elem` alive field,
  not (dx == 0 && dy == 0), x + dx >= 0, x + dx < width field, y + dy >= 0, y + dy < height field]

-- | Обработчик событий
handler :: Event -> Field -> Field
handler (EventKey (MouseButton LeftButton) Down _ mouse) field = addAlive (screenToCell mouse field) field
handler (EventKey (SpecialKey KeySpace) Down _ _) (Field l w h p) = Field l w h (not p)
handler (EventKey (SpecialKey KeyUp) Down _ _) (Field l w h p) = Field l w (h + 1) p
handler (EventKey (SpecialKey KeyDown) Down _ _) (Field l w h p)
  | h - 1 > 0 = Field (filter (\c -> snd c >= 0 && snd c < h - 1) l) w (h - 1) p
  | otherwise = Field l w h p
handler (EventKey (SpecialKey KeyRight) Down _ _) (Field l w h p) = Field l (w + 1) h p
handler (EventKey (SpecialKey KeyLeft) Down _ _) (Field l w h p)
  | w - 1 > 0 = Field (filter (\c -> fst c >= 0 && fst c < w - 1) l) (w - 1) h p 
  | otherwise = Field l w h p
handler (EventKey (Char 'c') Down _ _) (Field _ w h p) = Field [] w h p
handler _ field = field

-- | Добавление живой клетки на поле
addAlive :: Cell -> Field -> Field
addAlive (x, y) field
  | x >= 0 && x < width field && y >= 0 && y < height field && not ((x, y) `elem` alive field) = 
    Field ((x, y) : alive field) (width field) (height field) (pause field)
  | otherwise = Field (delete (x, y) (alive field)) (width field) (height field) (pause field)

-- | Преобразовать координаты в окне в координаты на игровом поле 
screenToCell :: Point -> Field -> Cell
screenToCell p field = ((round . (/ cellSize) . fst . invertViewPort (viewPort field)) p, 
  (round . (/ cellSize) . snd . invertViewPort (viewPort field)) p)

-- | Запустить игру 
startGame :: Field -> IO ()
startGame field = play window background fps field renderer handler oneStep
  where
    window = InWindow "Conway's Game of Life" (windowWidth, windowHeight) (100, 100)
    windowWidth = (round . fst . cellToScreen) (width field, height field) + round (3 * cellSize)
    windowHeight = (round . snd . cellToScreen) (width field, height field) + round (3 * cellSize)
    background = white
    fps = 10
