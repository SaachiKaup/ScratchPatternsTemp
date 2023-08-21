module ParseTurtles where 
import qualified IntermediateNotation as IN
import           Sound.Tidal.ParseBP
import           Sound.Tidal.Context
import           Text.ParserCombinators.Parsec

pRight :: MyParser (TPat IN.Turtle)
pRight = wrapPos $ do char 'r' <?> "r (Right)"
                      return (TPat_Atom Nothing $ IN.Right 1)

pLeft :: MyParser (TPat IN.Turtle)
pLeft = wrapPos $ do char 'l' <?> "l (Left)"
                     return (TPat_Atom Nothing $ IN.Left 1)

pForward :: MyParser (TPat IN.Turtle)
pForward = wrapPos $ do char 'f' <?> "f (Forward)"
                        return (TPat_Atom Nothing $ IN.Forward 1)

pTurtle :: MyParser (TPat IN.Turtle)
pTurtle = pRight <|> pLeft <|> pForward

instance Parseable IN.Turtle where
  tPatParser = pTurtle
  doEuclid = euclidOff
  getControl = error "not defined"

instance Enumerable IN.Turtle where
  fromTo a b = fastFromList [a,b]
  fromThenTo a b c = fastFromList [a,b,c]

--for testing
pat :: Pattern IN.Turtle
pat = "l f" :: Pattern IN.Turtle

