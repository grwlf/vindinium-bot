
module Imports (
    module Control.Arrow
  , module Control.Applicative
  , module Control.Concurrent
  , module Control.Concurrent.STM
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.State.Strict
  , module Control.Monad.Random
  , module Control.Monad.Reader
  , module Control.Monad.Identity
  , module Control.Break
  , module Control.Lens
  , module Data.Bits
  , module Data.Ratio
  , module Data.Tuple
  , module Data.List
  , module Data.Map.Strict
  , module Data.Maybe
  , module Data.Hashable
  , module Data.Monoid
  , module Data.Set
  , module Data.Function
  , module Data.Foldable
  , module Data.Text
  , module Data.HashSet
  , module Data.HashMap.Strict
  , module Data.Time.Clock
  , module Data.Time.Calendar
  , module Debug.Trace
  , module Prelude
  , module System.Random
  , module System.Random.Mersenne.Pure64
  , module System.Directory
  , module System.FilePath
  , module Text.Printf
  , module Text.Heredoc
  , module Text.Show.Pretty
  , module Imports
)

where

import Control.Arrow ((&&&),(***))
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Break
import Control.Lens (Lens, makeLenses, (%=), (^.), view, use, uses, _1, _2, _3, _4, _5, _6)
import Data.Bits
import Data.Ratio
import Data.Tuple
import Data.List hiding (break)
import Data.Map.Strict (Map, (!))
import Data.Set (Set,member)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Hashable
import Data.Monoid ((<>))
import Data.Foldable
import Data.Function
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Calendar
import Debug.Trace hiding(traceM)
import Prelude hiding(break)
import System.Random
import System.Random.Mersenne.Pure64
import System.Directory
import System.FilePath
import Text.Printf
import Text.Heredoc
import Text.Show.Pretty hiding(String)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

out :: (MonadIO m) => [Text] -> m ()
out = liftIO . Text.putStrLn . Text.concat

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

trace1 :: (Show a) => a -> a
trace1 a = trace (ppShow a) a

traceM :: (Monad m, Show a) => a -> m ()
traceM a = trace (ppShow a) (return ())

