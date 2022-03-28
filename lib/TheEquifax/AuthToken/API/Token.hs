{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TheEquifax.AuthToken.API.Token where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Data as P (Typeable)
import qualified Data.Proxy as P (Proxy (..))
import qualified Data.Text as T
import TheEquifax.AuthToken.Model (AuthTokenResponse, AuthTokenScope, scopeToUrl)
import TheEquifax.Core as Core
import TheEquifax.Core.Auth (AuthBasicOAuth20)
import TheEquifax.Core.MimeTypes
import Web.FormUrlEncoded (ToForm)
import Web.Internal.FormUrlEncoded (ToForm (toForm))
import qualified Prelude as P

data AuthTokenInput = AuthTokenInput
  { grantType :: T.Text, -- "client_credentials"
    scope :: AuthTokenScope
  }
  deriving (P.Show, P.Eq, P.Typeable)

instance A.ToJSON AuthTokenInput where
  toJSON AuthTokenInput {..} =
    A.object ["grant_type" .= grantType, "scope" .= scope]

instance ToForm AuthTokenInput where
  toForm AuthTokenInput {..} =
    [("grant_type", grantType), ("scope", scopeToUrl scope)]

mkAuthTokenInput :: AuthTokenScope -> AuthTokenInput
mkAuthTokenInput scope = AuthTokenInput {grantType = "client_credentials", scope}

-- * Operations

-- *** requestAithToken

-- | @POST \/report-requests@
--
-- AuthMethod: 'AuthBasicOAuth20'
requestAuthToken ::
  (Consumes GenerateAccessToken MimeFormUrlEncoded) =>
  AuthTokenScope ->
  TheEquifaxRequest GenerateAccessToken MimeFormUrlEncoded AuthTokenResponse MimeJSON
requestAuthToken scope =
  _mkRequest "POST" ["/v2/oauth/token"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicOAuth20)
    `setBodyParam` mkAuthTokenInput scope

data GenerateAccessToken

instance HasBodyParam GenerateAccessToken AuthTokenInput where
  setBodyParam req AuthTokenInput {..} =
    req `addForm` Core.toForm ("grant_type", grantType)
      `addForm` Core.toForm ("scope", scopeToUrl scope)

-- | @application/x-www-form-urlencoded@
instance Consumes GenerateAccessToken MimeFormUrlEncoded

-- | @application/json@
instance Produces GenerateAccessToken MimeJSON
