module Game where 

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort


-- | Состояние игрового поля
data Field = Field
  { alive :: [Cell] -- ^ живые клетки
  , width :: Int    -- ^ ширина игрового поля
  , height :: Int   -- ^ высота игрового поля
  , pause :: Bool   -- ^ флаг паузы игры
  }

-- | Размер каждой клетки поля при отрисовке
cellSize :: Float 
cellSize = 30

-- | Состояние поля в начале игры
initField :: Field
initField = Field [] 15 15 True

-- | Клетка поля
type Cell = (Int, Int)

-- | Проекция, для того чтобы поле отображалось по центру окна
viewPort :: ViewPort
viewPort = ViewPort (negate . (/ 2) . (subtract cellSize) . fst $ cellToScreen (width initField, height initField),
  negate . (/ 2) . (subtract cellSize) . snd $ cellToScreen (width initField, height initField)) 0 1

-- | Отобразить игровое поле
renderer :: Field -> Picture
renderer field = applyViewPortToPicture viewPort (pictures 
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

-- | Преобразовать координаты на игровом поле в координаты в окне
cellToScreen :: Cell -> Point
cellToScreen (x, y) = (fromIntegral x * cellSize, fromIntegral y * cellSize)

-- | Совершает один шаг игры 
oneStep :: Float -> Field -> Field
oneStep _ field
  | pause field == True = field
  | otherwise = applyRules field

-- | Применяет правила игры к полю
applyRules :: Field -> Field
applyRules = id

-- | Обработчик событий
handler :: Event -> Field -> Field
handler (EventKey (MouseButton LeftButton) Down _ mouse) field = addAlive (screenToCell mouse) field
handler (EventKey (SpecialKey KeySpace) Down _ _) (Field l x y p) = Field l x y (not p)
handler (EventKey (SpecialKey KeyUp) Down _ _) (Field l x y p) = Field l x (y + 1) p
handler (EventKey (SpecialKey KeyDown) Down _ _) (Field l x y p)
  | y - 1 > 0 = Field (filter (\c -> snd c >= 0 && snd c < y - 1) l) x (y - 1) p
  | otherwise = Field l x y p
handler (EventKey (SpecialKey KeyRight) Down _ _) (Field l x y p) = Field l (x + 1) y p
handler (EventKey (SpecialKey KeyLeft) Down _ _) (Field l x y p)
  | x - 1 > 0 = Field (filter (\c -> fst c >= 0 && fst c < x - 1) l) (x - 1) y p 
  | otherwise = Field l x y p
handler _ field = field

-- | Добавление живой клетки на поле
addAlive :: Cell -> Field -> Field
addAlive (x, y) field
  | x >= 0 && x < width field && y >= 0 && y < height field && not ((x, y) `elem` alive field) = 
    Field ((x, y) : alive field) (width field) (height field) (pause field)
  | otherwise = field

-- | Преобразовать координаты в окне в координаты на игровом поле 
screenToCell :: Point -> Cell
screenToCell p = ((round . (/ cellSize) . fst . invertViewPort viewPort) p, (round . (/ cellSize) . snd . invertViewPort viewPort) p)

-- | Запустить игру 
startGame :: Field -> IO ()
startGame field = play window background fps field renderer handler oneStep
  where
    window = InWindow "Good Window" (windowWidth, windowHeight) (200, 200)
    windowWidth = (round . fst . cellToScreen) (width initField, height initField) + round (2 * cellSize)
    windowHeight = (round . snd . cellToScreen) (width initField, height initField) + round (2 * cellSize)
    background = white
    fps = 1