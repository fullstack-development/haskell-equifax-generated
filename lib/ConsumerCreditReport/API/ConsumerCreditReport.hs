{-
   Consumer Credit Report

   # Introduction       Equifax’s Consumer Credit Report is the leading consumer credit resource enabling lenders to make faster, more informed credit-granting decisions, better manage their risk and maximize growth opportunities. The Consumer Credit Report unites the power of superior consumer dfata with best-in-class search and match logic capabilities to deliver higher match rates on more inquiries.    # Getting Started  1. **<a href=\"/user/applications\" target=\"_blank\">Create</a>** an application  2. **<a href=\"/user/applications\" target=\"_blank\">Subscribe</a>** to Consumer Credit Report API  3. **<a href=\"/products/consumer-credit-report\" target=\"_blank\">Explore</a>** the Sandbox environment mode # Promoting to UAT  To successfully submit test transactions through your application in the UAT environment, a test member number is required.  You may use your existing Equifax test member number(s).  Please contact your Equifax Account Representative if a new test member number is needed. The steps below can be used to test in the UAT environment.  Transactions generated using a test member number do not return identical results as a sandbox transaction.  A sandbox transaction will reflect everything that is exposed in the API whereas a test member number is configured to align with the specifications of your contract. Please contact us to ensure your test member number is configured to your expectations.      1. **<a href=\"/user/applications\" target=\"_blank\">Promote</a>** application to test  2. **<a href=\"/documentation\" target=\"_blank\">Update</a>** requests details for UAT  3. **<a href=\"/documentation\" target=\"_blank\">Test</a>** scenarios in UAT environment   # For More Details    * **<a href=\"/contact\" target=\"_blank\">Contact Us</a>** - Equifax Developer Center Support Website   * **ACRO Migration Support**       * phone: 1-888-407-0359; Option 2 followed by Option 5         * email:  BT.Acro.tech@equifax.com        

   OpenAPI Version: 3.0.0
   Consumer Credit Report API version: 1.0.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : ConsumerCreditReport.API.ConsumerCreditReport
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module ConsumerCreditReport.API.ConsumerCreditReport where

import ConsumerCreditReport.Core
import ConsumerCreditReport.MimeTypes
import ConsumerCreditReport.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** ConsumerCreditReport

-- *** requestConsumerCreditReport

-- | @POST \/reports\/credit-report@
-- 
-- AuthMethod: 'AuthBasicOAuth20'
-- 
requestConsumerCreditReport 
  :: (Consumes RequestConsumerCreditReport MimeJSON, MimeRender MimeJSON CreditReportRequest)
  => CreditReportRequest -- ^ "creditReportRequest"
  -> ConsumerCreditReportRequest RequestConsumerCreditReport MimeJSON CreditReportResponse MimeJSON
requestConsumerCreditReport creditReportRequest =
  _mkRequest "POST" ["/reports/credit-report"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicOAuth20)
    `setBodyParam` creditReportRequest

data RequestConsumerCreditReport 
instance HasBodyParam RequestConsumerCreditReport CreditReportRequest 

-- | @application/json@
instance Consumes RequestConsumerCreditReport MimeJSON

-- | @application/json@
instance Produces RequestConsumerCreditReport MimeJSON

