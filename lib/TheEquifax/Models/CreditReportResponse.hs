{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TheEquifax.Models.CreditReportResponse where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Data as P (Typeable)
import Data.Text (Text)
import TheEquifax.Models.ConsumerCreditReportEquifaxUSConsumerCreditReport (ConsumerCreditReportEquifaxUSConsumerCreditReport)
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (=<<), (>>=))
import qualified Prelude as P

import TheEquifax.Core
--

-- Abstract and unify type of CreditReportRespose from ConsumerCreditReport API and PrescreenOfOne API
--
--
--
--
--

-- ** CreditReportResponse

-- | CreditReportResponse
data CreditReportResponse = CreditReportResponse
  { -- | "status" - Transaction status. It could include some comments related to the transaction
    creditReportResponseStatus :: !(Maybe Text),
    -- | "consumers"
    creditReportResponseConsumers :: !(Maybe ConsumerCreditReport),
    -- | "links" - Returned when pdfCombo Indicator is set to &#39;Y&#39; or &#39;S&#39;. A separate link is returned for each credit report generated for the requested applicant(s). When multiple reports option is set to &#39;F&#39;, a maximum of 4 reports may be returned for each applicant.
    creditReportResponseLinks :: !(Maybe [CreditReportResponseLinks])
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON CreditReportResponse
instance A.FromJSON CreditReportResponse where
  parseJSON = A.withObject "CreditReportResponse" $ \o ->
    CreditReportResponse
      <$> (o .:? "status")
      <*> (o .:? "consumers")
      <*> (o .:? "links")

-- | ToJSON CreditReportResponse
instance A.ToJSON CreditReportResponse where
  toJSON CreditReportResponse {..} =
    _omitNulls
      [ "status" .= creditReportResponseStatus,
        "consumers" .= creditReportResponseConsumers,
        "links" .= creditReportResponseLinks
      ]

-- | Construct a value of type 'CreditReportResponse' (by applying it's required fields, if any)
mkCreditReportResponse ::
  CreditReportResponse
mkCreditReportResponse =
  CreditReportResponse
    { creditReportResponseStatus = Nothing,
      creditReportResponseConsumers = Nothing,
      creditReportResponseLinks = Nothing
    }

-- ** ConsumerCreditReport

-- | ConsumerCreditReport
newtype ConsumerCreditReport = ConsumerCreditReport
  { -- | "equifaxUSConsumerCreditReport"
    consumerCreditReportEquifaxUsConsumerCreditReport :: Maybe [ConsumerCreditReportEquifaxUSConsumerCreditReport]
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReport
instance A.FromJSON ConsumerCreditReport where
  parseJSON = A.withObject "ConsumerCreditReport" $ \o ->
    ConsumerCreditReport
      <$> (o .:? "equifaxUSConsumerCreditReport")

-- | ToJSON ConsumerCreditReport
instance A.ToJSON ConsumerCreditReport where
  toJSON ConsumerCreditReport {..} =
    _omitNulls
      [ "equifaxUSConsumerCreditReport" .= consumerCreditReportEquifaxUsConsumerCreditReport
      ]

-- | Construct a value of type 'ConsumerCreditReport' (by applying it's required fields, if any)
mkConsumerCreditReport ::
  ConsumerCreditReport
mkConsumerCreditReport =
  ConsumerCreditReport
    { consumerCreditReportEquifaxUsConsumerCreditReport = Nothing
    }

-- ** CreditReportResponseLinks

-- | CreditReportResponseLinks
data CreditReportResponseLinks = CreditReportResponseLinks
  { -- | "identifier" - identifies the credit report contained in the PDF. Corresponds to the JSON response equifaxUSConsumerCreditReport.identifier.
    creditReportResponseLinksIdentifier :: !(Maybe Text),
    -- | "type" - REST method to retrieve PDF
    creditReportResponseLinksType :: !(Maybe Text),
    -- | "href" - Link to retrieve PDF for report named in identifier
    creditReportResponseLinksHref :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON CreditReportResponseLinks
instance A.FromJSON CreditReportResponseLinks where
  parseJSON = A.withObject "CreditReportResponseLinks" $ \o ->
    CreditReportResponseLinks
      <$> (o .:? "identifier")
      <*> (o .:? "type")
      <*> (o .:? "href")

-- | ToJSON CreditReportResponseLinks
instance A.ToJSON CreditReportResponseLinks where
  toJSON CreditReportResponseLinks {..} =
    _omitNulls
      [ "identifier" .= creditReportResponseLinksIdentifier,
        "type" .= creditReportResponseLinksType,
        "href" .= creditReportResponseLinksHref
      ]

-- | Construct a value of type 'CreditReportResponseLinks' (by applying it's required fields, if any)
mkCreditReportResponseLinks ::
  CreditReportResponseLinks
mkCreditReportResponseLinks =
  CreditReportResponseLinks
    { creditReportResponseLinksIdentifier = Nothing,
      creditReportResponseLinksType = Nothing,
      creditReportResponseLinksHref = Nothing
    }
