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

module TheEquifax.Models.CreditorClassificationCode where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Data as P (Typeable)
import Data.Text (Text)
import TheEquifax.Core
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (=<<), (>>=))
import qualified Prelude as P

-- Abstract and unify type of CreditorClassificationCode from ConsumerCreditReport API and PrescreenOfOne API

-- ** CreditorClassificationCode

-- | CreditorClassificationCode
-- A general type of business for the original creditor and is only applicable for trades reported by Debt Buyers
data CreditorClassificationCode = CreditorClassificationCode
  { -- | "code" - Code value
    creditorClassificationCodeCode :: !(Maybe Text),
    -- | "description" - Business type of original creditor   - 01: RETAIL   - 02: MEDICAL/HEALTH CARE   - 03: OIL COMPANY   - 04: GOVERNMENT   - 05: PERSONAL SERVICES   - 06: INSURANCE   - 07: EDUCATIONAL   - 08: BANKING   - 09: RENTAL/LEASING   - 10: UTILITIES   - 11: CABLE/CELLULAR   - 12: FINANCIAL   - 13: CREDIT UNION   - 14: AUTOMOTIVE   - 15: CHECK GUARANTEE
    creditorClassificationCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON CreditorClassificationCode
instance A.FromJSON CreditorClassificationCode where
  parseJSON = A.withObject "CreditorClassificationCode" $ \o ->
    CreditorClassificationCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON CreditorClassificationCode
instance A.ToJSON CreditorClassificationCode where
  toJSON CreditorClassificationCode {..} =
    _omitNulls
      [ "code" .= creditorClassificationCodeCode,
        "description" .= creditorClassificationCodeDescription
      ]

-- | Construct a value of type 'CreditorClassificationCode' (by applying it's required fields, if any)
mkCreditorClassificationCode ::
  CreditorClassificationCode
mkCreditorClassificationCode =
  CreditorClassificationCode
    { creditorClassificationCodeCode = Nothing,
      creditorClassificationCodeDescription = Nothing
    }
