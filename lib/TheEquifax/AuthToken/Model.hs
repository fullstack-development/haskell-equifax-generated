{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TheEquifax.AuthToken.Model where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Data as P (Typeable)
import qualified Data.Text as T
import Prelude (Maybe, pure, ($), (.), (<$>), (<*>))
import qualified Prelude as P

data AuthTokenScope
  = Scope'PrescreenOfOne -- "https://api.equifax.com/business/prescreen-of-one/v1"
  | Scope'ConsumerCreditReport -- https://api.equifax.com/business/consumer-credit/v1
  | Scope'IdentityVerification -- "https://api.equifax.com/business/verifications/twn/v1/identity-verifications:r"
  deriving (P.Eq, P.Show, P.Read)

scopeToUrl :: AuthTokenScope -> T.Text
scopeToUrl Scope'PrescreenOfOne = "https://api.equifax.com/business/prescreen-of-one/v1"
scopeToUrl Scope'ConsumerCreditReport = "https://api.equifax.com/business/consumer-credit/v1"
scopeToUrl Scope'IdentityVerification = "https://api.equifax.com/business/verifications/twn/v1/identity-verifications:r"

instance A.FromJSON AuthTokenScope where
  parseJSON = A.withText "OAuthTokenScope" \case
    "https://api.equifax.com/business/prescreen-of-one/v1" -> pure Scope'PrescreenOfOne
    "https://api.equifax.com/business/consumer-credit/v1" -> pure Scope'ConsumerCreditReport
    "https://api.equifax.com/business/verifications/twn/v1/identity-verifications:r" -> pure Scope'IdentityVerification
    unknown -> A.prependFailure "Unknwon scope" (A.unexpected (A.String unknown))

instance A.ToJSON AuthTokenScope where
  toJSON = A.String . scopeToUrl

-- ** AuthTokenResponse

-- | AuthTokenResponse
data AuthTokenResponse = AuthTokenResponse
  { accessToken :: !T.Text,
    -- FIXME: I specifically made them optional
    --        to avoid affect of response or types mismatches
    tokenType :: !(Maybe T.Text), -- "Bearer",
    expiresIn :: !(Maybe P.Int), -- 1449,
    issuedAt :: !(Maybe T.Text), --- "1648203217802" -- TODO: parse it
    scope :: !(Maybe AuthTokenScope) -- "https://api.equifax.com/business/prescreen-of-one/v1"
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AuthTokenResponse
instance A.FromJSON AuthTokenResponse where
  parseJSON = A.withObject "OAuthTokenResponse" $ \o ->
    AuthTokenResponse
      <$> (o .: "access_token")
      <*> (o .:? "token_type")
      <*> (o .:? "expires_in")
      <*> (o .:? "issued_at")
      <*> (o .:? "scope")

-- | ToJSON AuthTokenResponse
instance A.ToJSON AuthTokenResponse where
  toJSON AuthTokenResponse {..} =
    A.object
      [ "access_token" .= accessToken,
        "token_type" .= tokenType,
        "expires_in" .= expiresIn,
        "issued_at" .= issuedAt,
        "scope" .= scope
      ]
