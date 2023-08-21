module GlossTurtles where
 
import Prelude hiding (Left, Right)
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Animate
import           Graphics.Gloss.Interface.IO.Simulate
import qualified Sound.Tidal.Context                 as T
import           ParseTurtles
import           IntermediateNotation
import           TidalEvents

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

displayWindow :: Display
displayWindow = (InWindow "Hello, World" (windowWidth, windowHeight) (0, 0))

simulationRate :: Int
simulationRate = 4

type Model = (Float, Float, Float)

initialModel :: Model
initialModel = (0, 0, pi/2)

apattern =  T.cat $ map pure [
                  PosTurtle { pos = (0, 0), move = Forward 10 }
                , PosTurtle { pos = (10, 0), move = Right 90  }
            ]

--mandala' = T.fastcat $ map pure [Forward 10, Right 45, Forward 20, Left 90, Forward 10]
mandala'' = T.slow "1 1 2 3 5 8" $ T.cat $ map pure [Forward 10, Right 45, Forward 20, Left 90, Forward 10]
--mandala = T.cat $ map pure [Forward 10, Right (pi/4), Forward 10, Left (pi/4)]
simpleMandala = T.cat $ map pure [Forward 10, Right (90)]

screenThread :: IO (MVar (T.Pattern Turtle))
screenThread = do
    mvPattern <- newMVar simpleMandala
    threadId <- forkIO $ simulatePattern mvPattern
    return mvPattern

simulatePattern :: (MVar (T.Pattern Turtle)) -> IO ()
simulatePattern mvPattern = 
    simulateIO
        displayWindow
        white
        simulationRate
        initialModel
        modelToPicture 
        (updatePicture mvPattern)

modelToPicture :: Model -> IO Picture
modelToPicture (x,y,_) = do
    return $ Line [(0, 0), (x, y)]

updatePicture :: MVar(T.Pattern Turtle) -> ViewPort -> Float -> Model -> IO Model
updatePicture mvPat _ t (x, y, theta) = do
    pat <- readMVar mvPat
    let values = patternValues pat
    let headvalue = head values
    let theta' = case headvalue of 
            Right a    -> theta - a
            Left a     -> theta + a
            Forward a  -> theta 
    print $ show t
    return $ ((x + cos (theta')), (y + sin (theta')), theta')
        
patternValues :: T.Pattern Turtle -> [Turtle]
patternValues pat = map T.value $ sortByWhole $ arcEvents pat (T.Arc 0 2)

myAnimate mvPattern = 
    animateIO 
         displayWindow
         white
        (picture mvPattern)
         controllerSetRedraw

timeArc :: Float -> T.ArcF Rational
timeArc t = T.Arc (toRational t) $ toRational $ t + 1 

picture :: MVar (T.Pattern Turtle) -> Float -> IO Picture
picture mvPattern t =
    do
       pat <- readMVar mvPattern
       let values = map T.value $ sortByWhole $ arcEvents
                        pat 
                        (timeArc t)
       print values
       return $ scale 20 20 $ drawLines values

drawLine :: (Float, Float) -> [Turtle] -> IO (Picture)
drawLine _ [] = do
    return mempty

drawLines :: [Turtle] -> Picture
drawLines []         = mempty
drawLines ((Left angle):ts) = rotate angle $ drawLines ts
drawLines ((Right angle):ts) = rotate (0 - angle) $ drawLines ts
drawLines ((Forward len):ts) = translate len 0 $ pictures [line [(0,0), (-len,0)], drawLines ts]

