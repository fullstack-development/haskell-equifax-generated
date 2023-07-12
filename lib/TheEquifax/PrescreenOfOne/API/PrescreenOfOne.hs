{-
   Prescreen of One

   # Introduction      Equifax’s Prescreen of One is a sales tool that is intended to entice the consumer to take a step toward opening a credit or loan account. Lenders can make offers of credit or insurance, in real time, in person or over the phone.    # Getting Started  1. **<a href=\"/user/applications\" target=\"_blank\">Create</a>** an application  2. **<a href=\"/user/applications\" target=\"_blank\">Subscribe</a>** to Prescreen of One API  3. **<a href=\"/products/consumer-credit-report\" target=\"_blank\">Explore</a>** the Sandbox environment  # Promoting to UAT  To successfully submit test transactions through your application in the UAT environment, a test member number is required.  You may use your existing Equifax test member number(s).  Please contact your Equifax Account Representative if a new test member number is needed. The steps below can be used to test in the UAT environment.  Transactions generated using a test member number do not return identical results as a sandbox transaction.  A sandbox transaction will reflect everything that is exposed in the API whereas a test member number is configured to align with the specifications of your contract. Please contact us to ensure your test member number is configured to your expectations.      1. **<a href=\"/user/applications\" target=\"_blank\">Promote</a>** application to test  2. **<a href=\"/documentation\" target=\"_blank\">Update</a>** requests details for UAT  3. **<a href=\"/documentation\" target=\"_blank\">Test</a>** scenarios in UAT environment   # For More Details    * **<a href=\"/contact\" target=\"_blank\">Contact Us</a>** - Equifax Developer Center Support Website   * **ACRO Migration Support**       * phone: 1-888-407-0359; Option 2 followed by Option 5         * email:  BT.Acro.tech@equifax.com

   OpenAPI Version: 3.0.0
   Prescreen of One API version: 1.0.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- |
-- Module : PrescreenOfOne.API.PrescreenOfOne
module TheEquifax.PrescreenOfOne.API.PrescreenOfOne where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (TypeRep, Typeable, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy (..))
import qualified Data.Set as Set
import qualified Data.String as P
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import GHC.Base ((<|>))
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import TheEquifax.Core
import TheEquifax.Core.Auth (AuthEquaifaxOAuth20Token)
import TheEquifax.Core.MimeTypes
import TheEquifax.Models.CreditReportResponse (CreditReportResponse)
import TheEquifax.PrescreenOfOne.Model as M (CreditReportRequest, EfxClientCorrelationId (EfxClientCorrelationId))
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (==), (>>=))
import qualified Prelude as P

-- * Operations

-- ** PrescreenOfOne

-- *** requestPrescreenOfOne

-- | @POST \/report-requests@
--
-- AuthMethod: 'AuthBasicOAuth20'
requestPrescreenOfOne ::
  (Consumes RequestPrescreenOfOne MimeJSON, MimeRender MimeJSON CreditReportRequest) =>
  -- | "creditReportRequest"
  CreditReportRequest ->
  TheEquifaxRequest RequestPrescreenOfOne MimeJSON CreditReportResponse MimeJSON
requestPrescreenOfOne creditReportRequest =
  _mkRequest "POST" ["/business/prescreen-of-one/v1/report-requests"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthEquaifaxOAuth20Token)
    `setBodyParam` creditReportRequest

data RequestPrescreenOfOne

instance HasBodyParam RequestPrescreenOfOne CreditReportRequest

-- | /Optional Param/ "efx-client-correlation-id" - ID provided by client to uniquely track service request
instance HasOptionalParam RequestPrescreenOfOne EfxClientCorrelationId where
  applyOptionalParam req (EfxClientCorrelationId xs) =
    req `addHeader` toHeader ("efx-client-correlation-id", xs)

-- | @application/json@
instance Consumes RequestPrescreenOfOne MimeJSON

-- | @application/json@
instance Produces RequestPrescreenOfOne MimeJSON
