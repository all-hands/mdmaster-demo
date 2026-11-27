module Mdmaster.Web.IdentitySpec (spec) where

import qualified Data.Text as T
import Mdmaster.Web.Identity (IdentityProvider (..), anonymousProvider)
import Network.HTTP.Types.Header (RequestHeaders)
import Test.Hspec

credentialed :: RequestHeaders
credentialed =
  [ ("Authorization", "Bearer secret-token")
  , ("Cookie", "session=abc123")
  , ("X-Forwarded-For", "203.0.113.7")
  , ("User-Agent", "spec-agent")
  ]

spec :: Spec
spec = describe "anonymousProvider" $ do
  it "labels every request the same, whatever it carries" $ do
    bare <- resolveIdentity anonymousProvider []
    loaded <- resolveIdentity anonymousProvider credentialed
    loaded `shouldBe` bare

  it "answers with a non-empty label" $ do
    label <- resolveIdentity anonymousProvider []
    label `shouldSatisfy` (not . T.null)

  it "answers with a label that is one token, safe as a log value" $ do
    label <- resolveIdentity anonymousProvider []
    label `shouldSatisfy` T.all (`notElem` [' ', '=', '\n', '\t'])

  it "echoes nothing of the request's headers" $ do
    label <- resolveIdentity anonymousProvider credentialed
    mapM_ (\needle -> label `shouldSatisfy` (not . T.isInfixOf needle))
      ["secret-token", "abc123", "203.0.113.7", "spec-agent"]
