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

module TheEquifax.Models.Address where

import qualified Control.Arrow as P (left)
import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Data as P (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import TheEquifax.Core
import TheEquifax.Core.MimeTypes
import qualified Web.HttpApiData as WH
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (=<<), (>>=))
import qualified Prelude as P

-- Abstract and unify type of Address from ConsumerCreditReport API and PrescreenOfOne API

-- ** Address

-- | Address
-- Subject's address(es) information
data Address = Address
  { -- | "addressType" - The address type:   - current: Current Address. A maximum of 1 current Address can be returned per report   - former: Former Address. A maximum of 1 former Address can be returned per report   - additional: Additional Address. A maximum of 8 additional Addresses can be returned per report
    addressAddressType :: !(Maybe E'AddressType),
    -- | "houseNumber" - Street Number/House number
    addressHouseNumber :: !(Maybe Text),
    -- | "streetName" - Street name
    addressStreetName :: !(Maybe Text),
    -- | "streetType" - Street type/Direction/Apartment number
    addressStreetType :: !(Maybe Text),
    -- | "cityName" - City name
    addressCityName :: !(Maybe Text),
    -- | "stateAbbreviation" - State abbreviation
    addressStateAbbreviation :: !(Maybe Text),
    -- | "zipCode" - Zip code
    addressZipCode :: !(Maybe Text),
    -- | "rentOwnBuy" - Rent/Own/Buy
    addressRentOwnBuy :: !(Maybe Text),
    -- | "sourceOfAddress"
    -- FIXME: returns Either Object or Array of Objects
    -- "addresses" : [ {
    --    "sourceOfAddress" : {
    --      "code" : "C",
    --      "description" : "EQUIFAX"
    --    },
    --  },
    --  {
    --    "sourceOfAddress" : [ {
    --      "code" : "C",
    --      "description" : "EQUIFAX"
    --    } ],
    --  },
    --  {
    --    "sourceOfAddress" : [ {
    --      "code" : "C",
    --      "description" : "EQUIFAX"
    --    } ]
    -- addressSourceOfAddress :: !(Maybe AddressSourceOfAddress),
    -- | "telephoneNumber" - Telephone number including a valid area code
    addressTelephoneNumber :: !(Maybe Text),
    -- | "sourceOfTelephoneNumber"
    addressSourceOfTelephoneNumber :: !(Maybe AddressSourceOfTelephoneNumber),
    -- | "addressVarianceIndicator"
    addressAddressVarianceIndicator :: !(Maybe AddressAddressVarianceIndicator),
    -- | "addressLine1" - Combination of address components
    addressAddressLine1 :: !(Maybe Text),
    -- | "dateFirstReported" - First date the address was reported to Equifax
    addressDateFirstReported :: !(Maybe Date),
    -- | "dateLastReported" - Last date the address was reported to Equifax
    addressDateLastReported :: !(Maybe Date),
    -- | "dateTelephoneReported" - Date the telephone number was reported to Equifax
    addressDateTelephoneReported :: !(Maybe Date)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Address
instance A.FromJSON Address where
  parseJSON = A.withObject "Address" $ \o ->
    Address
      <$> (o .:? "addressType")
      <*> (o .:? "houseNumber")
      <*> (o .:? "streetName")
      <*> (o .:? "streetType")
      <*> (o .:? "cityName")
      <*> (o .:? "stateAbbreviation")
      <*> (o .:? "zipCode")
      <*> (o .:? "rentOwnBuy")
      -- <*> (o .:? "sourceOfAddress")
      <*> (o .:? "telephoneNumber")
      <*> (o .:? "sourceOfTelephoneNumber")
      <*> (o .:? "addressVarianceIndicator")
      <*> (o .:? "addressLine1")
      <*> (o .:? "dateFirstReported")
      <*> (o .:? "dateLastReported")
      <*> (o .:? "dateTelephoneReported")

-- | ToJSON Address
instance A.ToJSON Address where
  toJSON Address {..} =
    _omitNulls
      [ "addressType" .= addressAddressType,
        "houseNumber" .= addressHouseNumber,
        "streetName" .= addressStreetName,
        "streetType" .= addressStreetType,
        "cityName" .= addressCityName,
        "stateAbbreviation" .= addressStateAbbreviation,
        "zipCode" .= addressZipCode,
        "rentOwnBuy" .= addressRentOwnBuy,
        -- "sourceOfAddress" .= addressSourceOfAddress,
        "telephoneNumber" .= addressTelephoneNumber,
        "sourceOfTelephoneNumber" .= addressSourceOfTelephoneNumber,
        "addressVarianceIndicator" .= addressAddressVarianceIndicator,
        "addressLine1" .= addressAddressLine1,
        "dateFirstReported" .= addressDateFirstReported,
        "dateLastReported" .= addressDateLastReported,
        "dateTelephoneReported" .= addressDateTelephoneReported
      ]

-- | Construct a value of type 'Address' (by applying it's required fields, if any)
mkAddress ::
  Address
mkAddress =
  Address
    { addressAddressType = Nothing,
      addressHouseNumber = Nothing,
      addressStreetName = Nothing,
      addressStreetType = Nothing,
      addressCityName = Nothing,
      addressStateAbbreviation = Nothing,
      addressZipCode = Nothing,
      addressRentOwnBuy = Nothing,
      -- addressSourceOfAddress = Nothing,
      addressTelephoneNumber = Nothing,
      addressSourceOfTelephoneNumber = Nothing,
      addressAddressVarianceIndicator = Nothing,
      addressAddressLine1 = Nothing,
      addressDateFirstReported = Nothing,
      addressDateLastReported = Nothing,
      addressDateTelephoneReported = Nothing
    }

-- ** AddressSourceOfAddress

-- | AddressSourceOfAddress
-- The source of address data
data AddressSourceOfAddress = AddressSourceOfAddress
  { -- | "code" - Code value
    addressSourceOfAddressCode :: !(Maybe Text),
    -- | "description" - Sources of address:   - C: EQUIFAX   - D: OTH/Sys-Sys   - T: AUT
    addressSourceOfAddressDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AddressSourceOfAddress
instance A.FromJSON AddressSourceOfAddress where
  parseJSON = A.withObject "AddressSourceOfAddress" $ \o ->
    AddressSourceOfAddress
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON AddressSourceOfAddress
instance A.ToJSON AddressSourceOfAddress where
  toJSON AddressSourceOfAddress {..} =
    _omitNulls
      [ "code" .= addressSourceOfAddressCode,
        "description" .= addressSourceOfAddressDescription
      ]

-- | Construct a value of type 'AddressSourceOfAddress' (by applying it's required fields, if any)
mkAddressSourceOfAddress ::
  AddressSourceOfAddress
mkAddressSourceOfAddress =
  AddressSourceOfAddress
    { addressSourceOfAddressCode = Nothing,
      addressSourceOfAddressDescription = Nothing
    }

-- ** AddressSourceOfTelephoneNumber

-- | AddressSourceOfTelephoneNumber
-- Source of Telephone data
data AddressSourceOfTelephoneNumber = AddressSourceOfTelephoneNumber
  { -- | "code" - Code value
    addressSourceOfTelephoneNumberCode :: !(Maybe Text),
    -- | "description" - Sources of telephone number:   - 1: Special Vendor        - 2: Equifax        - 3: OTH/Sys-Sys        - 4: Automated Update Indicator
    addressSourceOfTelephoneNumberDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AddressSourceOfTelephoneNumber
instance A.FromJSON AddressSourceOfTelephoneNumber where
  parseJSON = A.withObject "AddressSourceOfTelephoneNumber" $ \o ->
    AddressSourceOfTelephoneNumber
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON AddressSourceOfTelephoneNumber
instance A.ToJSON AddressSourceOfTelephoneNumber where
  toJSON AddressSourceOfTelephoneNumber {..} =
    _omitNulls
      [ "code" .= addressSourceOfTelephoneNumberCode,
        "description" .= addressSourceOfTelephoneNumberDescription
      ]

-- | Construct a value of type 'AddressSourceOfTelephoneNumber' (by applying it's required fields, if any)
mkAddressSourceOfTelephoneNumber ::
  AddressSourceOfTelephoneNumber
mkAddressSourceOfTelephoneNumber =
  AddressSourceOfTelephoneNumber
    { addressSourceOfTelephoneNumberCode = Nothing,
      addressSourceOfTelephoneNumberDescription = Nothing
    }

-- ** AddressAddressVarianceIndicator

-- | AddressAddressVarianceIndicator
-- The Address Variance Indicator will alert you to the differences between the address submitted current and other addresses displayed on the consumer report.The Address Variance Indicator will return a code, for each address displayed on the report, informing you of the degree to which the first address submitted in the inquiry matched the address on the Equifax database. Address Variance Indicator is an optional feature offered by Equifax.  Please contact your Equifax Sales Associate for additional information and activation.
data AddressAddressVarianceIndicator = AddressAddressVarianceIndicator
  { -- | "code" - Code value
    addressAddressVarianceIndicatorCode :: !(Maybe Text),
    -- | "description"
    addressAddressVarianceIndicatorDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AddressAddressVarianceIndicator
instance A.FromJSON AddressAddressVarianceIndicator where
  parseJSON = A.withObject "AddressAddressVarianceIndicator" $ \o ->
    AddressAddressVarianceIndicator
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON AddressAddressVarianceIndicator
instance A.ToJSON AddressAddressVarianceIndicator where
  toJSON AddressAddressVarianceIndicator {..} =
    _omitNulls
      [ "code" .= addressAddressVarianceIndicatorCode,
        "description" .= addressAddressVarianceIndicatorDescription
      ]

-- | Construct a value of type 'AddressAddressVarianceIndicator' (by applying it's required fields, if any)
mkAddressAddressVarianceIndicator ::
  AddressAddressVarianceIndicator
mkAddressAddressVarianceIndicator =
  AddressAddressVarianceIndicator
    { addressAddressVarianceIndicatorCode = Nothing,
      addressAddressVarianceIndicatorDescription = Nothing
    }

-- ** E'AddressType

-- | Enum of 'Text' .
-- The address type:   - current: Current Address. A maximum of 1 current Address can be returned per report   - former: Former Address. A maximum of 1 former Address can be returned per report   - additional: Additional Address. A maximum of 8 additional Addresses can be returned per report
data E'AddressType
  = -- | @"current"@
    E'AddressType'Current
  | -- | @"former"@
    E'AddressType'Former
  | -- | @"additional"@
    E'AddressType'Additional
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON E'AddressType where toJSON = A.toJSON . fromE'AddressType

instance A.FromJSON E'AddressType where parseJSON o = P.either P.fail (pure . P.id) . toE'AddressType =<< A.parseJSON o

instance WH.ToHttpApiData E'AddressType where toQueryParam = WH.toQueryParam . fromE'AddressType

instance WH.FromHttpApiData E'AddressType where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toE'AddressType

instance MimeRender MimeMultipartFormData E'AddressType where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'E'AddressType' enum
fromE'AddressType :: E'AddressType -> Text
fromE'AddressType = \case
  E'AddressType'Current -> "current"
  E'AddressType'Former -> "former"
  E'AddressType'Additional -> "additional"

-- | parse 'E'AddressType' enum
toE'AddressType :: Text -> P.Either String E'AddressType
toE'AddressType = \case
  "current" -> P.Right E'AddressType'Current
  "former" -> P.Right E'AddressType'Former
  "additional" -> P.Right E'AddressType'Additional
  s -> P.Left $ "toE'AddressType: enum parse failure: " P.++ P.show s
