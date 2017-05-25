{-# LANGUAGE ScopedTypeVariables #-}
module Network.Hub.Router.Pattern(
  runApp,newRouter,module X
  ) where

-- TODO: factor out duplicate code with RouterClass
import Network.Hub.Util


import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S
import qualified STMContainers.Multimap               as SMM
import Language.Quid2.Pattern as X

-- A Producer/Consumer protocol
-- Messages from producers are sent to consumers
--data FanInOut a = Producer | Consumer

-- TODO: allow the pattern to be changed dynamically:
-- Note: this creates an asymmetry between what goes up:
data Protocol v  = Value v
                 | Filter (Pattern WildCard)
-- And what comes down: v

-- A named router, messages are sent to all other agents
-- Example: a public chat line
data ClassFiltered = ClassFiltered Name (Pattern WildCard) deriving (Eq, Ord, Typeable ,Show, Generic)
type Name = AbsType

instance Binary ClassFiltered
instance HasModel ClassFiltered

instance Hashable ClassFiltered

type ClassFilteredState = SMM.Multimap Name Client -- Multiple clients per hub

