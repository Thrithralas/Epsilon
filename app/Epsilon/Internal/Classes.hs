module Epsilon.Internal.Classes where

import Data.Text
import Prelude hiding ( MonadFail(..) )
import qualified Prelude as P

class Monad m => MonadFail m where
    fail :: Text -> m a

instance MonadFail IO where
    fail = P.fail . unpack
