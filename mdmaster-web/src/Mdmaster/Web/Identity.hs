module Mdmaster.Web.Identity
  ( IdentityProvider (..)
  , anonymousProvider
  ) where

import Data.Text (Text)
import Network.HTTP.Types.Header (RequestHeaders)

newtype IdentityProvider = IdentityProvider
  { resolveIdentity :: RequestHeaders -> IO Text
  }

anonymousProvider :: IdentityProvider
anonymousProvider = IdentityProvider (\_ -> pure "anonymous")
