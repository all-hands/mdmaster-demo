module Mdmaster.Web.Headers
  ( securityHeaders
  , contentSecurityPolicy
  ) where

import Data.ByteString (ByteString)
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)

securityHeaders :: Middleware
securityHeaders = addHeaders
  [ ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "DENY")
  , ("Referrer-Policy", "no-referrer")
  , ("Content-Security-Policy", contentSecurityPolicy)
  ]

contentSecurityPolicy :: ByteString
contentSecurityPolicy =
  "default-src 'self'; img-src 'self'; style-src 'self'; script-src 'self'; \
  \object-src 'none'; base-uri 'self'; form-action 'self'; \
  \frame-ancestors 'none'"
