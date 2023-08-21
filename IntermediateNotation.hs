module IntermediateNotation where
import Prelude hiding (Left, Right)

data Turtle =     Left    Float
              | Right   Float
              | Forward Float 
              deriving (Show)

data PosTurtle = PosTurtle { 
                             pos :: (Float, Float)
                           , move :: Turtle 
                 }
              deriving (Show)
