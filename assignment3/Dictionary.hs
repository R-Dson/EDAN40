module Dictionary (T, empty, lookup) where
import Prelude hiding (lookup)
import qualified Prelude

newtype T a b = Dictionary [(a, b)] deriving (Show)

empty :: (Eq a, Ord a) => T a b
empty = Dictionary []

lookup a (Dictionary dictionary) = Prelude.lookup a dictionary