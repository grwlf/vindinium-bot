module Util where

import Control.Monad.Trans
import Prelude hiding (break)
import Data.Text (unpack, Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


out :: (MonadIO m) => [Text] -> m ()
out = liftIO . Text.putStrLn . Text.concat

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
