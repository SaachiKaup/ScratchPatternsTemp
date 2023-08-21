module TidalEvents where

import           Data.Maybe (fromJust)
import           Sound.Tidal.Context
import qualified Data.Map as M
import           Data.List
import           ParseTurtles
import           IntermediateNotation 

events0To1 :: Pattern a -> [Event a]
events0To1 pat = query (pat) (State (Arc 0 1) (M.empty))

wholeEvent :: EventF (ArcF Time) b -> ArcF Time
wholeEvent = fromJust . whole

--should be abstracted
compareOnWhole :: EventF (ArcF Time) b -> EventF (ArcF Time) b -> Ordering
compareOnWhole x x' = compare (wholeEvent x) (wholeEvent x')

sortByWhole :: [EventF (ArcF Time) b] -> [EventF (ArcF Time) b]
sortByWhole = sortBy (compareOnWhole) 

compareOnPart :: EventF (ArcF Time) b -> EventF (ArcF Time) b -> Ordering
compareOnPart x x' = compare (part x) (part x')

sortByPart :: [EventF (ArcF Time) b] -> [EventF (ArcF Time) b]
sortByPart = sortBy (compareOnPart) 

arcEvents :: Pattern a -> Arc -> [Event a]
arcEvents pat arc = queryArc pat arc

--Testing
evs = arcEvents pat (Arc 0 3)

