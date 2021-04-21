{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import ConsumerCreditReport.Model
import ConsumerCreditReport.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary APIErrorResponse where
  arbitrary = sized genAPIErrorResponse

genAPIErrorResponse :: Int -> Gen APIErrorResponse
genAPIErrorResponse n =
  APIErrorResponse
    <$> arbitrary -- aPIErrorResponseEfxErrorCode :: Double
    <*> arbitrary -- aPIErrorResponseDescription :: Text
    <*> arbitraryReducedMaybe n -- aPIErrorResponseAdditionalErrorDetails :: Maybe AdditionalErrorDetails
  
instance Arbitrary AccountDesignatorCode where
  arbitrary = sized genAccountDesignatorCode

genAccountDesignatorCode :: Int -> Gen AccountDesignatorCode
genAccountDesignatorCode n =
  AccountDesignatorCode
    <$> arbitraryReducedMaybe n -- accountDesignatorCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- accountDesignatorCodeDescription :: Maybe Text
  
instance Arbitrary AccountTypeCode where
  arbitrary = sized genAccountTypeCode

genAccountTypeCode :: Int -> Gen AccountTypeCode
genAccountTypeCode n =
  AccountTypeCode
    <$> arbitraryReducedMaybe n -- accountTypeCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- accountTypeCodeDescription :: Maybe Text
  
instance Arbitrary AdditionalErrorDetails where
  arbitrary = sized genAdditionalErrorDetails

genAdditionalErrorDetails :: Int -> Gen AdditionalErrorDetails
genAdditionalErrorDetails n =
  AdditionalErrorDetails
    <$> arbitrary -- additionalErrorDetailsStatus :: Double
    <*> arbitraryReducedMaybe n -- additionalErrorDetailsErrors :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- additionalErrorDetailsCode :: Maybe Double
    <*> arbitraryReducedMaybe n -- additionalErrorDetailsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- additionalErrorDetailsTimeStamp :: Maybe Text
  
instance Arbitrary Address where
  arbitrary = sized genAddress

genAddress :: Int -> Gen Address
genAddress n =
  Address
    <$> arbitraryReducedMaybe n -- addressAddressType :: Maybe E'AddressType
    <*> arbitraryReducedMaybe n -- addressHouseNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressStreetName :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressStreetType :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressCityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressStateAbbreviation :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressZipCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressRentOwnBuy :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressSourceOfAddress :: Maybe AddressSourceOfAddress
    <*> arbitraryReducedMaybe n -- addressTelephoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressSourceOfTelephoneNumber :: Maybe AddressSourceOfTelephoneNumber
    <*> arbitraryReducedMaybe n -- addressAddressVarianceIndicator :: Maybe AddressAddressVarianceIndicator
    <*> arbitraryReducedMaybe n -- addressAddressLine1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressDateFirstReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- addressDateLastReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- addressDateTelephoneReported :: Maybe Date
  
instance Arbitrary AddressAddressVarianceIndicator where
  arbitrary = sized genAddressAddressVarianceIndicator

genAddressAddressVarianceIndicator :: Int -> Gen AddressAddressVarianceIndicator
genAddressAddressVarianceIndicator n =
  AddressAddressVarianceIndicator
    <$> arbitraryReducedMaybe n -- addressAddressVarianceIndicatorCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressAddressVarianceIndicatorDescription :: Maybe Text
  
instance Arbitrary AddressRequest where
  arbitrary = sized genAddressRequest

genAddressRequest :: Int -> Gen AddressRequest
genAddressRequest n =
  AddressRequest
    <$> arbitrary -- addressRequestIdentifier :: Text
    <*> arbitraryReducedMaybe n -- addressRequestHouseNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressRequestQuadrant :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressRequestStreetName :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressRequestStreetType :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressRequestApartmentNumber :: Maybe Text
    <*> arbitrary -- addressRequestCity :: Text
    <*> arbitrary -- addressRequestState :: Text
    <*> arbitraryReducedMaybe n -- addressRequestZip :: Maybe Text
  
instance Arbitrary AddressSourceOfAddress where
  arbitrary = sized genAddressSourceOfAddress

genAddressSourceOfAddress :: Int -> Gen AddressSourceOfAddress
genAddressSourceOfAddress n =
  AddressSourceOfAddress
    <$> arbitraryReducedMaybe n -- addressSourceOfAddressCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressSourceOfAddressDescription :: Maybe Text
  
instance Arbitrary AddressSourceOfTelephoneNumber where
  arbitrary = sized genAddressSourceOfTelephoneNumber

genAddressSourceOfTelephoneNumber :: Int -> Gen AddressSourceOfTelephoneNumber
genAddressSourceOfTelephoneNumber n =
  AddressSourceOfTelephoneNumber
    <$> arbitraryReducedMaybe n -- addressSourceOfTelephoneNumberCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressSourceOfTelephoneNumberDescription :: Maybe Text
  
instance Arbitrary AlertContactAddress where
  arbitrary = sized genAlertContactAddress

genAlertContactAddress :: Int -> Gen AlertContactAddress
genAlertContactAddress n =
  AlertContactAddress
    <$> arbitraryReducedMaybe n -- alertContactAddressAddressLine1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- alertContactAddressAddressLine2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- alertContactAddressCityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- alertContactAddressStateAbbreviation :: Maybe Text
    <*> arbitraryReducedMaybe n -- alertContactAddressZipCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- alertContactAddressCountryCode :: Maybe Text
  
instance Arbitrary Bankruptcy where
  arbitrary = sized genBankruptcy

genBankruptcy :: Int -> Gen Bankruptcy
genBankruptcy n =
  Bankruptcy
    <$> arbitraryReducedMaybe n -- bankruptcyCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyType :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyFiler :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyIndustryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyCurrentIntentOrDispositionCode :: Maybe BankruptcyCurrentIntentOrDispositionCode
    <*> arbitraryReducedMaybe n -- bankruptcyNarrativeCodes :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- bankruptcyRawNarrativeCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- bankruptcyCaseNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyDispositionDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyDateFiled :: Maybe Date
    <*> arbitraryReducedMaybe n -- bankruptcyCurrentDispositionDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- bankruptcyVerifiedDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- bankruptcyPriorIntentOrDispositionCode :: Maybe BankruptcyPriorIntentOrDispositionCode
    <*> arbitraryReducedMaybe n -- bankruptcyDateReported :: Maybe Date
  
instance Arbitrary BankruptcyCurrentIntentOrDispositionCode where
  arbitrary = sized genBankruptcyCurrentIntentOrDispositionCode

genBankruptcyCurrentIntentOrDispositionCode :: Int -> Gen BankruptcyCurrentIntentOrDispositionCode
genBankruptcyCurrentIntentOrDispositionCode n =
  BankruptcyCurrentIntentOrDispositionCode
    <$> arbitraryReducedMaybe n -- bankruptcyCurrentIntentOrDispositionCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyCurrentIntentOrDispositionCodeDescription :: Maybe Text
  
instance Arbitrary BankruptcyPriorIntentOrDispositionCode where
  arbitrary = sized genBankruptcyPriorIntentOrDispositionCode

genBankruptcyPriorIntentOrDispositionCode :: Int -> Gen BankruptcyPriorIntentOrDispositionCode
genBankruptcyPriorIntentOrDispositionCode n =
  BankruptcyPriorIntentOrDispositionCode
    <$> arbitraryReducedMaybe n -- bankruptcyPriorIntentOrDispositionCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- bankruptcyPriorIntentOrDispositionCodeDescription :: Maybe Text
  
instance Arbitrary Collection where
  arbitrary = sized genCollection

genCollection :: Int -> Gen Collection
genCollection n =
  Collection
    <$> arbitraryReducedMaybe n -- collectionIndustryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionClientNameOrNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionStatusCode :: Maybe CollectionStatusCode
    <*> arbitraryReducedMaybe n -- collectionNarrativeCodes :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- collectionRawNarrativeCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- collectionIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionDateReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- collectionDateAssigned :: Maybe Date
    <*> arbitraryReducedMaybe n -- collectionOriginalAmount :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionStatusDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- collectionBalance :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionLastPaymentDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- collectionDateOfFirstDelinquency :: Maybe Date
    <*> arbitraryReducedMaybe n -- collectionAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionAccountDesignatorCode :: Maybe AccountDesignatorCode
    <*> arbitraryReducedMaybe n -- collectionCreditorClassificationCode :: Maybe CreditorClassificationCode
  
instance Arbitrary CollectionStatusCode where
  arbitrary = sized genCollectionStatusCode

genCollectionStatusCode :: Int -> Gen CollectionStatusCode
genCollectionStatusCode n =
  CollectionStatusCode
    <$> arbitraryReducedMaybe n -- collectionStatusCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- collectionStatusCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReport where
  arbitrary = sized genConsumerCreditReport

genConsumerCreditReport :: Int -> Gen ConsumerCreditReport
genConsumerCreditReport n =
  ConsumerCreditReport
    <$> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUsConsumerCreditReport :: Maybe [ConsumerCreditReportEquifaxUSConsumerCreditReport]
  
instance Arbitrary ConsumerCreditReportAlertContacts where
  arbitrary = sized genConsumerCreditReportAlertContacts

genConsumerCreditReportAlertContacts :: Int -> Gen ConsumerCreditReportAlertContacts
genConsumerCreditReportAlertContacts n =
  ConsumerCreditReportAlertContacts
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsAlertType :: Maybe ConsumerCreditReportAlertType
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsDateReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsEffectiveDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsTelephoneNumbers :: Maybe [ConsumerCreditReportTelephoneNumbers]
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsAddress :: Maybe AlertContactAddress
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertContactsAdditionalInformation :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAlertType where
  arbitrary = sized genConsumerCreditReportAlertType

genConsumerCreditReportAlertType :: Int -> Gen ConsumerCreditReportAlertType
genConsumerCreditReportAlertType n =
  ConsumerCreditReportAlertType
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlertTypeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlertTypeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAlternateDataSources where
  arbitrary = sized genConsumerCreditReportAlternateDataSources

genConsumerCreditReportAlternateDataSources :: Int -> Gen ConsumerCreditReportAlternateDataSources
genConsumerCreditReportAlternateDataSources n =
  ConsumerCreditReportAlternateDataSources
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage :: Maybe [ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage]
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower :: Maybe ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesNorthAmericanLink :: Maybe ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdAlerts :: Maybe ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdv2Alerts :: Maybe ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode

genConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode :: Int -> Gen ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
genConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode n =
  ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeErrorCodes :: Maybe [ConsumerCreditReportAlternateDataSourcesErrorCodes]
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage

genConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage :: Int -> Gen ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
genConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage n =
  ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerReferenceNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageErrorType :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageAlternateDataSourceCode :: Maybe [ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode]
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesErrorCodes where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesErrorCodes

genConsumerCreditReportAlternateDataSourcesErrorCodes :: Int -> Gen ConsumerCreditReportAlternateDataSourcesErrorCodes
genConsumerCreditReportAlternateDataSourcesErrorCodes n =
  ConsumerCreditReportAlternateDataSourcesErrorCodes
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesErrorCodesVerbiage :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesErrorCodesErrorText :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts

genConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts :: Int -> Gen ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
genConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts n =
  ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNonRegulatedIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsHitNohitIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsDisclaimer :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsFinalAssessmentFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsAuthorizedUserVelocityFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsIdDiscrepancyFlag :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfAuthorizedUsers :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfTerminatedUsers :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts

genConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts :: Int -> Gen ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
genConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts n =
  ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsNonRegulatedIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsHitNohitIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsSyntheticIdVer2 :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower

genConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower :: Int -> Gen ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
genConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower n =
  ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerRegulatedIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerDisclaimer :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerCoveredBorrowerStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerInsufficientDataProvidedForMatch :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerReferralContactNumber :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAlternateDataSourcesNorthAmericanLink where
  arbitrary = sized genConsumerCreditReportAlternateDataSourcesNorthAmericanLink

genConsumerCreditReportAlternateDataSourcesNorthAmericanLink :: Int -> Gen ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
genConsumerCreditReportAlternateDataSourcesNorthAmericanLink n =
  ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
    <$> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesNorthAmericanLinkRegulatedIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesNorthAmericanLinkHitNohitIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAlternateDataSourcesNorthAmericanLinkIntl5FffConsumerReport :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAttributes where
  arbitrary = sized genConsumerCreditReportAttributes

genConsumerCreditReportAttributes :: Int -> Gen ConsumerCreditReportAttributes
genConsumerCreditReportAttributes n =
  ConsumerCreditReportAttributes
    <$> arbitraryReducedMaybe n -- consumerCreditReportAttributesIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAttributesValue :: Maybe Text
  
instance Arbitrary ConsumerCreditReportAttributes1 where
  arbitrary = sized genConsumerCreditReportAttributes1

genConsumerCreditReportAttributes1 :: Int -> Gen ConsumerCreditReportAttributes1
genConsumerCreditReportAttributes1 n =
  ConsumerCreditReportAttributes1
    <$> arbitraryReducedMaybe n -- consumerCreditReportAttributes1ModelNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAttributes1NumberOfVariableFields :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportAttributes1Attributes :: Maybe [ConsumerCreditReportAttributes]
  
instance Arbitrary ConsumerCreditReportConsumerReferralLocation where
  arbitrary = sized genConsumerCreditReportConsumerReferralLocation

genConsumerCreditReportConsumerReferralLocation :: Int -> Gen ConsumerCreditReportConsumerReferralLocation
genConsumerCreditReportConsumerReferralLocation n =
  ConsumerCreditReportConsumerReferralLocation
    <$> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationBureauCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationBureauName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationAddress :: Maybe ConsumerCreditReportConsumerReferralLocationAddress
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationTelephoneNumber :: Maybe ConsumerCreditReportConsumerReferralLocationTelephoneNumber
  
instance Arbitrary ConsumerCreditReportConsumerReferralLocationAddress where
  arbitrary = sized genConsumerCreditReportConsumerReferralLocationAddress

genConsumerCreditReportConsumerReferralLocationAddress :: Int -> Gen ConsumerCreditReportConsumerReferralLocationAddress
genConsumerCreditReportConsumerReferralLocationAddress n =
  ConsumerCreditReportConsumerReferralLocationAddress
    <$> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationAddressPrimaryAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationAddressSecondaryAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationAddressCityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationAddressStateAbbreviation :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationAddressZipCode :: Maybe Text
  
instance Arbitrary ConsumerCreditReportConsumerReferralLocationTelephoneNumber where
  arbitrary = sized genConsumerCreditReportConsumerReferralLocationTelephoneNumber

genConsumerCreditReportConsumerReferralLocationTelephoneNumber :: Int -> Gen ConsumerCreditReportConsumerReferralLocationTelephoneNumber
genConsumerCreditReportConsumerReferralLocationTelephoneNumber n =
  ConsumerCreditReportConsumerReferralLocationTelephoneNumber
    <$> arbitraryReducedMaybe n -- consumerCreditReportConsumerReferralLocationTelephoneNumberTelephoneNumber :: Maybe Text
  
instance Arbitrary ConsumerCreditReportConsumerStatements where
  arbitrary = sized genConsumerCreditReportConsumerStatements

genConsumerCreditReportConsumerStatements :: Int -> Gen ConsumerCreditReportConsumerStatements
genConsumerCreditReportConsumerStatements n =
  ConsumerCreditReportConsumerStatements
    <$> arbitraryReducedMaybe n -- consumerCreditReportConsumerStatementsDateReported :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerStatementsDatePurged :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportConsumerStatementsStatement :: Maybe Text
  
instance Arbitrary ConsumerCreditReportEDASIndicatorCode where
  arbitrary = sized genConsumerCreditReportEDASIndicatorCode

genConsumerCreditReportEDASIndicatorCode :: Int -> Gen ConsumerCreditReportEDASIndicatorCode
genConsumerCreditReportEDASIndicatorCode n =
  ConsumerCreditReportEDASIndicatorCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportEDASIndicatorCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEDASIndicatorCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportEDASRegionalIndicatorCode where
  arbitrary = sized genConsumerCreditReportEDASRegionalIndicatorCode

genConsumerCreditReportEDASRegionalIndicatorCode :: Int -> Gen ConsumerCreditReportEDASRegionalIndicatorCode
genConsumerCreditReportEDASRegionalIndicatorCode n =
  ConsumerCreditReportEDASRegionalIndicatorCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportEDASRegionalIndicatorCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEDASRegionalIndicatorCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportEmployments where
  arbitrary = sized genConsumerCreditReportEmployments

genConsumerCreditReportEmployments :: Int -> Gen ConsumerCreditReportEmployments
genConsumerCreditReportEmployments n =
  ConsumerCreditReportEmployments
    <$> arbitraryReducedMaybe n -- consumerCreditReportEmploymentsIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEmploymentsOccupation :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEmploymentsEmployer :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEmploymentsDateLastReported :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEmploymentsDateFirstReported :: Maybe Text
  
instance Arbitrary ConsumerCreditReportEquifaxUSConsumerCreditReport where
  arbitrary = sized genConsumerCreditReportEquifaxUSConsumerCreditReport

genConsumerCreditReportEquifaxUSConsumerCreditReport :: Int -> Gen ConsumerCreditReportEquifaxUSConsumerCreditReport
genConsumerCreditReportEquifaxUSConsumerCreditReport n =
  ConsumerCreditReportEquifaxUSConsumerCreditReport
    <$> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportCustomerReferenceNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportMultipleReportIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportEcoaInquiryType :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportNumberOfMonthsToCountInquiries :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportHitCode :: Maybe ConsumerCreditReportHitCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportFileSinceDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportLastActivityDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportReportDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportSubjectName :: Maybe ConsumerCreditReportSubjectName
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportSubjectSocialNum :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportBirthDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportAge :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportNameMatchFlags :: Maybe ConsumerCreditReportNameMatchFlags
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportLinkIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportDoNotCombineIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportAddressDiscrepancyIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportFraudSocialNumAlertCode :: Maybe ConsumerCreditReportFraudSocialNumAlertCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportFraudVictimIndicator :: Maybe ConsumerCreditReportFraudVictimIndicator
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportAddresses :: Maybe [Address]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportIdentityScan :: Maybe ConsumerCreditReportIdentityScan
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportFormerNames :: Maybe [ConsumerCreditReportFormerNames]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportDeathDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportEmployments :: Maybe [ConsumerCreditReportEmployments]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportOtherIdentification :: Maybe [ConsumerCreditReportOtherIdentification]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportBankruptcies :: Maybe [Bankruptcy]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportCollections :: Maybe [Collection]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportFileIdentificationNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportAlertContacts :: Maybe [ConsumerCreditReportAlertContacts]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportTrades :: Maybe [Trade]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportInquiries :: Maybe [ConsumerCreditReportInquiries]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportConsumerStatements :: Maybe [ConsumerCreditReportConsumerStatements]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportModels :: Maybe [ConsumerCreditReportModels]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportOnlineDirectory :: Maybe [ConsumerCreditReportOnlineDirectory]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportIdentification :: Maybe ConsumerCreditReportIdentification
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportAttributes :: Maybe [ConsumerCreditReportAttributes1]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportOnlineGeoCode :: Maybe [ConsumerCreditReportOnlineGeoCode]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportOfacAlerts :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralLocation :: Maybe ConsumerCreditReportConsumerReferralLocation
    <*> arbitraryReducedMaybe n -- consumerCreditReportEquifaxUSConsumerCreditReportAlternateDataSources :: Maybe ConsumerCreditReportAlternateDataSources
  
instance Arbitrary ConsumerCreditReportFICOScoreIndicatorCode where
  arbitrary = sized genConsumerCreditReportFICOScoreIndicatorCode

genConsumerCreditReportFICOScoreIndicatorCode :: Int -> Gen ConsumerCreditReportFICOScoreIndicatorCode
genConsumerCreditReportFICOScoreIndicatorCode n =
  ConsumerCreditReportFICOScoreIndicatorCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportFICOScoreIndicatorCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportFICOScoreIndicatorCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportFormerNames where
  arbitrary = sized genConsumerCreditReportFormerNames

genConsumerCreditReportFormerNames :: Int -> Gen ConsumerCreditReportFormerNames
genConsumerCreditReportFormerNames n =
  ConsumerCreditReportFormerNames
    <$> arbitraryReducedMaybe n -- consumerCreditReportFormerNamesLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportFormerNamesFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportFormerNamesMiddleInitial :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportFormerNamesSuffix :: Maybe Text
  
instance Arbitrary ConsumerCreditReportFraudSocialNumAlertCode where
  arbitrary = sized genConsumerCreditReportFraudSocialNumAlertCode

genConsumerCreditReportFraudSocialNumAlertCode :: Int -> Gen ConsumerCreditReportFraudSocialNumAlertCode
genConsumerCreditReportFraudSocialNumAlertCode n =
  ConsumerCreditReportFraudSocialNumAlertCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportFraudSocialNumAlertCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportFraudSocialNumAlertCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportFraudVictimIndicator where
  arbitrary = sized genConsumerCreditReportFraudVictimIndicator

genConsumerCreditReportFraudVictimIndicator :: Int -> Gen ConsumerCreditReportFraudVictimIndicator
genConsumerCreditReportFraudVictimIndicator n =
  ConsumerCreditReportFraudVictimIndicator
    <$> arbitraryReducedMaybe n -- consumerCreditReportFraudVictimIndicatorCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportFraudVictimIndicatorDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportHitCode where
  arbitrary = sized genConsumerCreditReportHitCode

genConsumerCreditReportHitCode :: Int -> Gen ConsumerCreditReportHitCode
genConsumerCreditReportHitCode n =
  ConsumerCreditReportHitCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportHitCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportHitCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportIdentification where
  arbitrary = sized genConsumerCreditReportIdentification

genConsumerCreditReportIdentification :: Int -> Gen ConsumerCreditReportIdentification
genConsumerCreditReportIdentification n =
  ConsumerCreditReportIdentification
    <$> arbitraryReducedMaybe n -- consumerCreditReportIdentificationSubjectAge :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationSubjectSocialNum :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationSocialNumConfirmed :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationSocialMatchFlags :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationInquirySocialNum :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationInquirySocialNumStateIssued :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationInquirySocialNumYearIssued :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationInquirySocialNumYearOfDeath :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationInquirySocialNumStateOfDeath :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentificationSocialNumMatch :: Maybe Text
  
instance Arbitrary ConsumerCreditReportIdentityScan where
  arbitrary = sized genConsumerCreditReportIdentityScan

genConsumerCreditReportIdentityScan :: Int -> Gen ConsumerCreditReportIdentityScan
genConsumerCreditReportIdentityScan n =
  ConsumerCreditReportIdentityScan
    <$> arbitraryReducedMaybe n -- consumerCreditReportIdentityScanAlertCodes :: Maybe [ConsumerCreditReportIdentityScanAlertCodes]
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentityScanIdentityScanRegulated :: Maybe Text
  
instance Arbitrary ConsumerCreditReportIdentityScanAlertCodes where
  arbitrary = sized genConsumerCreditReportIdentityScanAlertCodes

genConsumerCreditReportIdentityScanAlertCodes :: Int -> Gen ConsumerCreditReportIdentityScanAlertCodes
genConsumerCreditReportIdentityScanAlertCodes n =
  ConsumerCreditReportIdentityScanAlertCodes
    <$> arbitraryReducedMaybe n -- consumerCreditReportIdentityScanAlertCodesCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportIdentityScanAlertCodesDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportInquiries where
  arbitrary = sized genConsumerCreditReportInquiries

genConsumerCreditReportInquiries :: Int -> Gen ConsumerCreditReportInquiries
genConsumerCreditReportInquiries n =
  ConsumerCreditReportInquiries
    <$> arbitraryReducedMaybe n -- consumerCreditReportInquiriesType :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportInquiriesIndustryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportInquiriesInquiryDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportInquiriesCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportInquiriesCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportInquiriesExpandedAccountTypeOrInquiryIntent :: Maybe AccountTypeCode
  
instance Arbitrary ConsumerCreditReportInquiryKeyFactor where
  arbitrary = sized genConsumerCreditReportInquiryKeyFactor

genConsumerCreditReportInquiryKeyFactor :: Int -> Gen ConsumerCreditReportInquiryKeyFactor
genConsumerCreditReportInquiryKeyFactor n =
  ConsumerCreditReportInquiryKeyFactor
    <$> arbitraryReducedMaybe n -- consumerCreditReportInquiryKeyFactorCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportInquiryKeyFactorDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportModels where
  arbitrary = sized genConsumerCreditReportModels

genConsumerCreditReportModels :: Int -> Gen ConsumerCreditReportModels
genConsumerCreditReportModels n =
  ConsumerCreditReportModels
    <$> arbitraryReducedMaybe n -- consumerCreditReportModelsType :: Maybe E'Type
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsModelNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsFicoScoreIndicatorCode :: Maybe ConsumerCreditReportFICOScoreIndicatorCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsScore :: Maybe Int
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsReasons :: Maybe [ConsumerCreditReportReasons]
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsInquiryKeyFactor :: Maybe ConsumerCreditReportInquiryKeyFactor
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsRiskBasedPricingOrModel :: Maybe ConsumerCreditReportRiskBasedPricingOrModel
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsRejects :: Maybe [ConsumerCreditReportRejects]
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsEdasRegionalIndicatorCode :: Maybe ConsumerCreditReportEDASRegionalIndicatorCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsEdasIndicatorCode :: Maybe ConsumerCreditReportEDASIndicatorCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsModelIdorScorecard :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsScoreNumberOrMarketMaxIndustryCode :: Maybe ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportModelsNumericScoreIndicator :: Maybe Text
  
instance Arbitrary ConsumerCreditReportNameMatchFlags where
  arbitrary = sized genConsumerCreditReportNameMatchFlags

genConsumerCreditReportNameMatchFlags :: Int -> Gen ConsumerCreditReportNameMatchFlags
genConsumerCreditReportNameMatchFlags n =
  ConsumerCreditReportNameMatchFlags
    <$> arbitraryReducedMaybe n -- consumerCreditReportNameMatchFlagsFirstNameMatchFlag :: Maybe E'FirstNameMatchFlag
    <*> arbitraryReducedMaybe n -- consumerCreditReportNameMatchFlagsLastNameMatchFlag :: Maybe E'FirstNameMatchFlag
    <*> arbitraryReducedMaybe n -- consumerCreditReportNameMatchFlagsMiddleNameMatchFlag :: Maybe E'FirstNameMatchFlag
    <*> arbitraryReducedMaybe n -- consumerCreditReportNameMatchFlagsSuffixMatchFlag :: Maybe E'FirstNameMatchFlag
  
instance Arbitrary ConsumerCreditReportOnlineDirectory where
  arbitrary = sized genConsumerCreditReportOnlineDirectory

genConsumerCreditReportOnlineDirectory :: Int -> Gen ConsumerCreditReportOnlineDirectory
genConsumerCreditReportOnlineDirectory n =
  ConsumerCreditReportOnlineDirectory
    <$> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryTelephoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryAddressLine1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryAddressLine2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryStateAbbreviation :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineDirectoryZipCode :: Maybe Text
  
instance Arbitrary ConsumerCreditReportOnlineGeoCode where
  arbitrary = sized genConsumerCreditReportOnlineGeoCode

genConsumerCreditReportOnlineGeoCode :: Int -> Gen ConsumerCreditReportOnlineGeoCode
genConsumerCreditReportOnlineGeoCode n =
  ConsumerCreditReportOnlineGeoCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoSmsaCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoStateCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoCountyCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoCensusTract :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoSuffix :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoBlockGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeStreetNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeStreetName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeStreetTypeOrDirection :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeGeoSmsa5DigitCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeStateAbbreviation :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeZipCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeTypeOfAddress :: Maybe ConsumerCreditReportTypeOfAddress
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeReturnCode1 :: Maybe ConsumerCreditReportReturnCode1
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeReturnCode2 :: Maybe ConsumerCreditReportReturnCode2
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeReturnCode3 :: Maybe ConsumerCreditReportReturnCode3
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeReturnCode4 :: Maybe ConsumerCreditReportReturnCode4
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeMicroVisionCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOnlineGeoCodeMicroVisionReturnCode :: Maybe Text
  
instance Arbitrary ConsumerCreditReportOtherIdentification where
  arbitrary = sized genConsumerCreditReportOtherIdentification

genConsumerCreditReportOtherIdentification :: Int -> Gen ConsumerCreditReportOtherIdentification
genConsumerCreditReportOtherIdentification n =
  ConsumerCreditReportOtherIdentification
    <$> arbitraryReducedMaybe n -- consumerCreditReportOtherIdentificationDateReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- consumerCreditReportOtherIdentificationTypeCode :: Maybe ConsumerCreditReportTypeCode
    <*> arbitraryReducedMaybe n -- consumerCreditReportOtherIdentificationIdentificationNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportOtherIdentificationReasonCode :: Maybe ConsumerCreditReportReasonCode
  
instance Arbitrary ConsumerCreditReportReasonCode where
  arbitrary = sized genConsumerCreditReportReasonCode

genConsumerCreditReportReasonCode :: Int -> Gen ConsumerCreditReportReasonCode
genConsumerCreditReportReasonCode n =
  ConsumerCreditReportReasonCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportReasonCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportReasonCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportReasons where
  arbitrary = sized genConsumerCreditReportReasons

genConsumerCreditReportReasons :: Int -> Gen ConsumerCreditReportReasons
genConsumerCreditReportReasons n =
  ConsumerCreditReportReasons
    <$> arbitraryReducedMaybe n -- consumerCreditReportReasonsCode :: Maybe Text
  
instance Arbitrary ConsumerCreditReportRejects where
  arbitrary = sized genConsumerCreditReportRejects

genConsumerCreditReportRejects :: Int -> Gen ConsumerCreditReportRejects
genConsumerCreditReportRejects n =
  ConsumerCreditReportRejects
    <$> arbitraryReducedMaybe n -- consumerCreditReportRejectsCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportRejectsDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportReturnCode1 where
  arbitrary = sized genConsumerCreditReportReturnCode1

genConsumerCreditReportReturnCode1 :: Int -> Gen ConsumerCreditReportReturnCode1
genConsumerCreditReportReturnCode1 n =
  ConsumerCreditReportReturnCode1
    <$> arbitraryReducedMaybe n -- consumerCreditReportReturnCode1Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportReturnCode1Description :: Maybe Text
  
instance Arbitrary ConsumerCreditReportReturnCode2 where
  arbitrary = sized genConsumerCreditReportReturnCode2

genConsumerCreditReportReturnCode2 :: Int -> Gen ConsumerCreditReportReturnCode2
genConsumerCreditReportReturnCode2 n =
  ConsumerCreditReportReturnCode2
    <$> arbitraryReducedMaybe n -- consumerCreditReportReturnCode2Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportReturnCode2Description :: Maybe Text
  
instance Arbitrary ConsumerCreditReportReturnCode3 where
  arbitrary = sized genConsumerCreditReportReturnCode3

genConsumerCreditReportReturnCode3 :: Int -> Gen ConsumerCreditReportReturnCode3
genConsumerCreditReportReturnCode3 n =
  ConsumerCreditReportReturnCode3
    <$> arbitraryReducedMaybe n -- consumerCreditReportReturnCode3Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportReturnCode3Description :: Maybe Text
  
instance Arbitrary ConsumerCreditReportReturnCode4 where
  arbitrary = sized genConsumerCreditReportReturnCode4

genConsumerCreditReportReturnCode4 :: Int -> Gen ConsumerCreditReportReturnCode4
genConsumerCreditReportReturnCode4 n =
  ConsumerCreditReportReturnCode4
    <$> arbitraryReducedMaybe n -- consumerCreditReportReturnCode4Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportReturnCode4Description :: Maybe Text
  
instance Arbitrary ConsumerCreditReportRiskBasedPricingOrModel where
  arbitrary = sized genConsumerCreditReportRiskBasedPricingOrModel

genConsumerCreditReportRiskBasedPricingOrModel :: Int -> Gen ConsumerCreditReportRiskBasedPricingOrModel
genConsumerCreditReportRiskBasedPricingOrModel n =
  ConsumerCreditReportRiskBasedPricingOrModel
    <$> arbitraryReducedMaybe n -- consumerCreditReportRiskBasedPricingOrModelPercentage :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportRiskBasedPricingOrModelLowRange :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportRiskBasedPricingOrModelHighRange :: Maybe Text
  
instance Arbitrary ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode where
  arbitrary = sized genConsumerCreditReportScoreNumberOrMarketMaxIndustryCode

genConsumerCreditReportScoreNumberOrMarketMaxIndustryCode :: Int -> Gen ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
genConsumerCreditReportScoreNumberOrMarketMaxIndustryCode n =
  ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportScoreNumberOrMarketMaxIndustryCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportScoreNumberOrMarketMaxIndustryCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportSubjectName where
  arbitrary = sized genConsumerCreditReportSubjectName

genConsumerCreditReportSubjectName :: Int -> Gen ConsumerCreditReportSubjectName
genConsumerCreditReportSubjectName n =
  ConsumerCreditReportSubjectName
    <$> arbitraryReducedMaybe n -- consumerCreditReportSubjectNameFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportSubjectNameLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportSubjectNameMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportSubjectNameSuffix :: Maybe Text
  
instance Arbitrary ConsumerCreditReportTelephoneNumberType where
  arbitrary = sized genConsumerCreditReportTelephoneNumberType

genConsumerCreditReportTelephoneNumberType :: Int -> Gen ConsumerCreditReportTelephoneNumberType
genConsumerCreditReportTelephoneNumberType n =
  ConsumerCreditReportTelephoneNumberType
    <$> arbitraryReducedMaybe n -- consumerCreditReportTelephoneNumberTypeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportTelephoneNumberTypeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportTelephoneNumbers where
  arbitrary = sized genConsumerCreditReportTelephoneNumbers

genConsumerCreditReportTelephoneNumbers :: Int -> Gen ConsumerCreditReportTelephoneNumbers
genConsumerCreditReportTelephoneNumbers n =
  ConsumerCreditReportTelephoneNumbers
    <$> arbitraryReducedMaybe n -- consumerCreditReportTelephoneNumbersTelephoneNumberType :: Maybe ConsumerCreditReportTelephoneNumberType
    <*> arbitraryReducedMaybe n -- consumerCreditReportTelephoneNumbersCountryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportTelephoneNumbersTelephoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportTelephoneNumbersExtension :: Maybe Text
  
instance Arbitrary ConsumerCreditReportTypeCode where
  arbitrary = sized genConsumerCreditReportTypeCode

genConsumerCreditReportTypeCode :: Int -> Gen ConsumerCreditReportTypeCode
genConsumerCreditReportTypeCode n =
  ConsumerCreditReportTypeCode
    <$> arbitraryReducedMaybe n -- consumerCreditReportTypeCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportTypeCodeDescription :: Maybe Text
  
instance Arbitrary ConsumerCreditReportTypeOfAddress where
  arbitrary = sized genConsumerCreditReportTypeOfAddress

genConsumerCreditReportTypeOfAddress :: Int -> Gen ConsumerCreditReportTypeOfAddress
genConsumerCreditReportTypeOfAddress n =
  ConsumerCreditReportTypeOfAddress
    <$> arbitraryReducedMaybe n -- consumerCreditReportTypeOfAddressCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- consumerCreditReportTypeOfAddressDescription :: Maybe Text
  
instance Arbitrary CreditReportRequest where
  arbitrary = sized genCreditReportRequest

genCreditReportRequest :: Int -> Gen CreditReportRequest
genCreditReportRequest n =
  CreditReportRequest
    <$> arbitraryReduced n -- creditReportRequestConsumers :: CreditReportRequestConsumers
    <*> arbitraryReducedMaybe n -- creditReportRequestExternalDataSources :: Maybe CreditReportRequestExternalDataSources
    <*> arbitraryReducedMaybe n -- creditReportRequestCustomerReferenceIdentifier :: Maybe Text
    <*> arbitraryReduced n -- creditReportRequestCustomerConfiguration :: CreditReportRequestCustomerConfiguration
  
instance Arbitrary CreditReportRequestConsumers where
  arbitrary = sized genCreditReportRequestConsumers

genCreditReportRequestConsumers :: Int -> Gen CreditReportRequestConsumers
genCreditReportRequestConsumers n =
  CreditReportRequestConsumers
    <$> arbitraryReduced n -- creditReportRequestConsumersName :: [CreditReportRequestConsumersName]
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersSocialNum :: Maybe [CreditReportRequestConsumersSocialNum]
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersDateOfBirth :: Maybe Date
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersAge :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersAddresses :: Maybe [AddressRequest]
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersPhoneNumbers :: Maybe [CreditReportRequestConsumersPhoneNumbers]
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersEmployments :: Maybe CreditReportRequestConsumersEmployments
  
instance Arbitrary CreditReportRequestConsumersEmployments where
  arbitrary = sized genCreditReportRequestConsumersEmployments

genCreditReportRequestConsumersEmployments :: Int -> Gen CreditReportRequestConsumersEmployments
genCreditReportRequestConsumersEmployments n =
  CreditReportRequestConsumersEmployments
    <$> arbitraryReducedMaybe n -- creditReportRequestConsumersEmploymentsOccupation :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersEmploymentsEmployerName :: Maybe Text
  
instance Arbitrary CreditReportRequestConsumersName where
  arbitrary = sized genCreditReportRequestConsumersName

genCreditReportRequestConsumersName :: Int -> Gen CreditReportRequestConsumersName
genCreditReportRequestConsumersName n =
  CreditReportRequestConsumersName
    <$> arbitrary -- creditReportRequestConsumersNameIdentifier :: Text
    <*> arbitrary -- creditReportRequestConsumersNameFirstName :: Text
    <*> arbitrary -- creditReportRequestConsumersNameLastName :: Text
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersNameMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportRequestConsumersNameSuffix :: Maybe Text
  
instance Arbitrary CreditReportRequestConsumersPhoneNumbers where
  arbitrary = sized genCreditReportRequestConsumersPhoneNumbers

genCreditReportRequestConsumersPhoneNumbers :: Int -> Gen CreditReportRequestConsumersPhoneNumbers
genCreditReportRequestConsumersPhoneNumbers n =
  CreditReportRequestConsumersPhoneNumbers
    <$> arbitrary -- creditReportRequestConsumersPhoneNumbersIdentifier :: Text
    <*> arbitrary -- creditReportRequestConsumersPhoneNumbersNumber :: Text
  
instance Arbitrary CreditReportRequestConsumersSocialNum where
  arbitrary = sized genCreditReportRequestConsumersSocialNum

genCreditReportRequestConsumersSocialNum :: Int -> Gen CreditReportRequestConsumersSocialNum
genCreditReportRequestConsumersSocialNum n =
  CreditReportRequestConsumersSocialNum
    <$> arbitrary -- creditReportRequestConsumersSocialNumIdentifier :: Text
    <*> arbitrary -- creditReportRequestConsumersSocialNumNumber :: Text
  
instance Arbitrary CreditReportRequestCustomerConfiguration where
  arbitrary = sized genCreditReportRequestCustomerConfiguration

genCreditReportRequestCustomerConfiguration :: Int -> Gen CreditReportRequestCustomerConfiguration
genCreditReportRequestCustomerConfiguration n =
  CreditReportRequestCustomerConfiguration
    <$> arbitraryReducedMaybe n -- creditReportRequestCustomerConfigurationEquifaxUsConsumerCreditReport :: Maybe EquifaxUSConsumerCreditRequest
  
instance Arbitrary CreditReportRequestExternalDataSources where
  arbitrary = sized genCreditReportRequestExternalDataSources

genCreditReportRequestExternalDataSources :: Int -> Gen CreditReportRequestExternalDataSources
genCreditReportRequestExternalDataSources n =
  CreditReportRequestExternalDataSources
    <$> arbitraryReducedMaybe n -- creditReportRequestExternalDataSourcesAlternateDataSources :: Maybe CreditReportRequestExternalDataSourcesAlternateDataSources
  
instance Arbitrary CreditReportRequestExternalDataSourcesAlternateDataSources where
  arbitrary = sized genCreditReportRequestExternalDataSourcesAlternateDataSources

genCreditReportRequestExternalDataSourcesAlternateDataSources :: Int -> Gen CreditReportRequestExternalDataSourcesAlternateDataSources
genCreditReportRequestExternalDataSourcesAlternateDataSources n =
  CreditReportRequestExternalDataSourcesAlternateDataSources
    <$> arbitrary -- creditReportRequestExternalDataSourcesAlternateDataSourcesConsumerReportIndicator :: Bool
    <*> arbitraryReducedMaybe n -- creditReportRequestExternalDataSourcesAlternateDataSourcesCustomerOrchestrationCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportRequestExternalDataSourcesAlternateDataSourcesCustomerOrganizationCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportRequestExternalDataSourcesAlternateDataSourcesDataProviderTag :: Maybe Text
  
instance Arbitrary CreditReportResponse where
  arbitrary = sized genCreditReportResponse

genCreditReportResponse :: Int -> Gen CreditReportResponse
genCreditReportResponse n =
  CreditReportResponse
    <$> arbitraryReducedMaybe n -- creditReportResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportResponseConsumers :: Maybe ConsumerCreditReport
    <*> arbitraryReducedMaybe n -- creditReportResponseLinks :: Maybe [CreditReportResponseLinks]
  
instance Arbitrary CreditReportResponseLinks where
  arbitrary = sized genCreditReportResponseLinks

genCreditReportResponseLinks :: Int -> Gen CreditReportResponseLinks
genCreditReportResponseLinks n =
  CreditReportResponseLinks
    <$> arbitraryReducedMaybe n -- creditReportResponseLinksIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportResponseLinksType :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditReportResponseLinksHref :: Maybe Text
  
instance Arbitrary CreditorClassificationCode where
  arbitrary = sized genCreditorClassificationCode

genCreditorClassificationCode :: Int -> Gen CreditorClassificationCode
genCreditorClassificationCode n =
  CreditorClassificationCode
    <$> arbitraryReducedMaybe n -- creditorClassificationCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- creditorClassificationCodeDescription :: Maybe Text
  
instance Arbitrary EquifaxUSConsumerCreditRequest where
  arbitrary = sized genEquifaxUSConsumerCreditRequest

genEquifaxUSConsumerCreditRequest :: Int -> Gen EquifaxUSConsumerCreditRequest
genEquifaxUSConsumerCreditRequest n =
  EquifaxUSConsumerCreditRequest
    <$> arbitrary -- equifaxUSConsumerCreditRequestMemberNumber :: Text
    <*> arbitrary -- equifaxUSConsumerCreditRequestSecurityCode :: Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestCodeDescriptionRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestEndUserInformation :: Maybe EquifaxUSConsumerCreditRequestEndUserInformation
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestProductCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestProductInformation :: Maybe EquifaxUSConsumerCreditRequestProductInformation
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestModels :: Maybe [EquifaxUSConsumerCreditRequestModels]
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestCustomerCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestMultipleReportIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestEcoaInquiryType :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestOptionalFeatureCode :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestPdfComboIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestVendorIdentificationCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestRiskModelCodeOnly :: Maybe Text
  
instance Arbitrary EquifaxUSConsumerCreditRequestEndUserInformation where
  arbitrary = sized genEquifaxUSConsumerCreditRequestEndUserInformation

genEquifaxUSConsumerCreditRequestEndUserInformation :: Int -> Gen EquifaxUSConsumerCreditRequestEndUserInformation
genEquifaxUSConsumerCreditRequestEndUserInformation n =
  EquifaxUSConsumerCreditRequestEndUserInformation
    <$> arbitrary -- equifaxUSConsumerCreditRequestEndUserInformationEndUsersName :: Text
    <*> arbitrary -- equifaxUSConsumerCreditRequestEndUserInformationPermissiblePurposeCode :: Text
  
instance Arbitrary EquifaxUSConsumerCreditRequestModels where
  arbitrary = sized genEquifaxUSConsumerCreditRequestModels

genEquifaxUSConsumerCreditRequestModels :: Int -> Gen EquifaxUSConsumerCreditRequestModels
genEquifaxUSConsumerCreditRequestModels n =
  EquifaxUSConsumerCreditRequestModels
    <$> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestModelsIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestModelsModelField :: Maybe [Text]
  
instance Arbitrary EquifaxUSConsumerCreditRequestProductInformation where
  arbitrary = sized genEquifaxUSConsumerCreditRequestProductInformation

genEquifaxUSConsumerCreditRequestProductInformation :: Int -> Gen EquifaxUSConsumerCreditRequestProductInformation
genEquifaxUSConsumerCreditRequestProductInformation n =
  EquifaxUSConsumerCreditRequestProductInformation
    <$> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber :: Maybe [EquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber]
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestProductInformationCoApplicantDateOfBirth :: Maybe Date
  
instance Arbitrary EquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber where
  arbitrary = sized genEquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber

genEquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber :: Int -> Gen EquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber
genEquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber n =
  EquifaxUSConsumerCreditRequestProductInformationDriverLicenseNumber
    <$> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestProductInformationDriverLicenseNumberIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- equifaxUSConsumerCreditRequestProductInformationDriverLicenseNumberDriverLicenseNumber :: Maybe Text
  
instance Arbitrary Trade where
  arbitrary = sized genTrade

genTrade :: Int -> Gen Trade
genTrade n =
  Trade
    <$> arbitraryReducedMaybe n -- tradeAutomatedUpdateIndicator :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeMonthsReviewed :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeAccountDesignator :: Maybe AccountDesignatorCode
    <*> arbitraryReducedMaybe n -- tradeAccountNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeThirtyDayCounter :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeSixtyDayCounter :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeNinetyDayCounter :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradePreviousHighRate1 :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradePreviousHighDate1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePreviousHighRate2 :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradePreviousHighDate2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePreviousHighRate3 :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradePreviousHighDate3 :: Maybe Text
    <*> arbitraryReducedMaybe n -- trade24monthPaymentHistory :: Maybe [Trade24MonthPaymentHistory]
    <*> arbitraryReducedMaybe n -- tradeCustomerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeCustomerNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeDateReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeDateOpened :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeHighCredit :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeCreditLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeBalance :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradePastDueAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradePortfolioTypeCode :: Maybe TradePortfolioTypeCode
    <*> arbitraryReducedMaybe n -- tradeRateStatusCode :: Maybe TradeRateStatusCode
    <*> arbitraryReducedMaybe n -- tradeRate :: Maybe TradeRate
    <*> arbitraryReducedMaybe n -- tradeLastActivityDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeNarrativeCodes :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- tradeRawNarrativeCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- tradeAccountTypeCode :: Maybe AccountTypeCode
    <*> arbitraryReducedMaybe n -- tradeLastPaymentDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeClosedDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeDateMajorDelinquencyFirstReported :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeActualPaymentAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeScheduledPaymentAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeTermsFrequencyCode :: Maybe TradeTermsFrequencyCode
    <*> arbitraryReducedMaybe n -- tradeTermsDurationCode :: Maybe TradeTermsDurationCode
    <*> arbitraryReducedMaybe n -- tradePurchasedFromOrSoldCreditorIndicator :: Maybe TradePurchasedFromOrSoldCreditorIndicator
    <*> arbitraryReducedMaybe n -- tradePurchasedFromOrSoldCreditorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeCreditorClassificationCode :: Maybe CreditorClassificationCode
    <*> arbitraryReducedMaybe n -- tradeActivityDesignatorCode :: Maybe TradeActivityDesignatorCode
    <*> arbitraryReducedMaybe n -- tradeOriginalChargeOffAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDeferredPaymentStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeBallonPaymentAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeBallonPaymentDueDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeMortgageIdNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePaymentHistory1to24 :: Maybe [TradePaymentHistory1to24]
    <*> arbitraryReducedMaybe n -- tradePaymentHistory25to36 :: Maybe [TradePaymentHistory25to36]
    <*> arbitraryReducedMaybe n -- tradePaymentHistory37to48 :: Maybe [TradePaymentHistory37to48]
    <*> arbitraryReducedMaybe n -- tradePreviousHighRatePaymentHistory :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePreviousHighDatePaymentHistory :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeDimensionsDataStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeDimensionsNumberOfMonths :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeDimension :: Maybe [TradeDimension]
  
instance Arbitrary Trade24MonthPaymentHistory where
  arbitrary = sized genTrade24MonthPaymentHistory

genTrade24MonthPaymentHistory :: Int -> Gen Trade24MonthPaymentHistory
genTrade24MonthPaymentHistory n =
  Trade24MonthPaymentHistory
    <$> arbitraryReducedMaybe n -- trade24MonthPaymentHistoryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- trade24MonthPaymentHistoryDescription :: Maybe Text
  
instance Arbitrary TradeActivityDesignatorCode where
  arbitrary = sized genTradeActivityDesignatorCode

genTradeActivityDesignatorCode :: Int -> Gen TradeActivityDesignatorCode
genTradeActivityDesignatorCode n =
  TradeActivityDesignatorCode
    <$> arbitraryReducedMaybe n -- tradeActivityDesignatorCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeActivityDesignatorCodeDescription :: Maybe Text
  
instance Arbitrary TradeDimension where
  arbitrary = sized genTradeDimension

genTradeDimension :: Int -> Gen TradeDimension
genTradeDimension n =
  TradeDimension
    <$> arbitraryReducedMaybe n -- tradeDimensionDimensionsBalance :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsActualPaymentAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsScheduledPaymentAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsLastPaymentDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsHighCredit :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsCreditLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsPastDueAmount :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsNarrativeCodes :: Maybe [A.Value]
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsRawNarrativeCodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsAccountDesignatorCode :: Maybe AccountDesignatorCode
    <*> arbitraryReducedMaybe n -- tradeDimensionDimensionsAccountTypeCode :: Maybe AccountTypeCode
  
instance Arbitrary TradePaymentHistory1to24 where
  arbitrary = sized genTradePaymentHistory1to24

genTradePaymentHistory1to24 :: Int -> Gen TradePaymentHistory1to24
genTradePaymentHistory1to24 n =
  TradePaymentHistory1to24
    <$> arbitraryReducedMaybe n -- tradePaymentHistory1to24Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePaymentHistory1to24Description :: Maybe Text
  
instance Arbitrary TradePaymentHistory25to36 where
  arbitrary = sized genTradePaymentHistory25to36

genTradePaymentHistory25to36 :: Int -> Gen TradePaymentHistory25to36
genTradePaymentHistory25to36 n =
  TradePaymentHistory25to36
    <$> arbitraryReducedMaybe n -- tradePaymentHistory25to36Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePaymentHistory25to36Description :: Maybe Text
  
instance Arbitrary TradePaymentHistory37to48 where
  arbitrary = sized genTradePaymentHistory37to48

genTradePaymentHistory37to48 :: Int -> Gen TradePaymentHistory37to48
genTradePaymentHistory37to48 n =
  TradePaymentHistory37to48
    <$> arbitraryReducedMaybe n -- tradePaymentHistory37to48Code :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePaymentHistory37to48Description :: Maybe Text
  
instance Arbitrary TradePortfolioTypeCode where
  arbitrary = sized genTradePortfolioTypeCode

genTradePortfolioTypeCode :: Int -> Gen TradePortfolioTypeCode
genTradePortfolioTypeCode n =
  TradePortfolioTypeCode
    <$> arbitraryReducedMaybe n -- tradePortfolioTypeCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePortfolioTypeCodeDescription :: Maybe Text
  
instance Arbitrary TradePurchasedFromOrSoldCreditorIndicator where
  arbitrary = sized genTradePurchasedFromOrSoldCreditorIndicator

genTradePurchasedFromOrSoldCreditorIndicator :: Int -> Gen TradePurchasedFromOrSoldCreditorIndicator
genTradePurchasedFromOrSoldCreditorIndicator n =
  TradePurchasedFromOrSoldCreditorIndicator
    <$> arbitraryReducedMaybe n -- tradePurchasedFromOrSoldCreditorIndicatorCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradePurchasedFromOrSoldCreditorIndicatorDescription :: Maybe Text
  
instance Arbitrary TradeRate where
  arbitrary = sized genTradeRate

genTradeRate :: Int -> Gen TradeRate
genTradeRate n =
  TradeRate
    <$> arbitraryReducedMaybe n -- tradeRateCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- tradeRateDescription :: Maybe Text
  
instance Arbitrary TradeRateStatusCode where
  arbitrary = sized genTradeRateStatusCode

genTradeRateStatusCode :: Int -> Gen TradeRateStatusCode
genTradeRateStatusCode n =
  TradeRateStatusCode
    <$> arbitraryReducedMaybe n -- tradeRateStatusCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeRateStatusCodeDescription :: Maybe Text
  
instance Arbitrary TradeTermsDurationCode where
  arbitrary = sized genTradeTermsDurationCode

genTradeTermsDurationCode :: Int -> Gen TradeTermsDurationCode
genTradeTermsDurationCode n =
  TradeTermsDurationCode
    <$> arbitraryReducedMaybe n -- tradeTermsDurationCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeTermsDurationCodeDescription :: Maybe Text
  
instance Arbitrary TradeTermsFrequencyCode where
  arbitrary = sized genTradeTermsFrequencyCode

genTradeTermsFrequencyCode :: Int -> Gen TradeTermsFrequencyCode
genTradeTermsFrequencyCode n =
  TradeTermsFrequencyCode
    <$> arbitraryReducedMaybe n -- tradeTermsFrequencyCodeCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- tradeTermsFrequencyCodeDescription :: Maybe Text
  



instance Arbitrary E'AddressType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ErrorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FirstNameMatchFlag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

