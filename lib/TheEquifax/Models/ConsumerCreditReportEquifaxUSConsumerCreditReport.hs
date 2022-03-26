{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TheEquifax.Models.ConsumerCreditReportEquifaxUSConsumerCreditReport where

import qualified Control.Arrow as P (left)
import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Data as P (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import TheEquifax.Core
import TheEquifax.Core.MimeTypes
import TheEquifax.Models.Address
import TheEquifax.Models.CreditorClassificationCode
import TheEquifax.Models.Trade
import qualified Web.HttpApiData as WH
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (=<<), (>>=))
import qualified Prelude as P

-- Abstract and unify type of ConsumerCreditReportEquifaxUSConsumerCreditReport   from ConsumerCreditReport API and PrescreenOfOne API
--
--
--
--

-- ** ConsumerCreditReportEquifaxUSConsumerCreditReport

-- | ConsumerCreditReportEquifaxUSConsumerCreditReport
data ConsumerCreditReportEquifaxUSConsumerCreditReport = ConsumerCreditReportEquifaxUSConsumerCreditReport
  { -- | "identifier"
    consumerCreditReportEquifaxUSConsumerCreditReportIdentifier :: !(Maybe Text),
    -- | "customerReferenceNumber" - This field will return a value sent in the request
    consumerCreditReportEquifaxUSConsumerCreditReportCustomerReferenceNumber :: !(Maybe Text),
    -- | "customerNumber" - This field returns the same value provided in the request
    consumerCreditReportEquifaxUSConsumerCreditReportCustomerNumber :: !(Maybe Text),
    -- | "consumerReferralCode" - Credit Reporting Agency Number
    consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralCode :: !(Maybe Text),
    -- | "multipleReportIndicator" - Indicates if the report contains more than 1 credit report file
    consumerCreditReportEquifaxUSConsumerCreditReportMultipleReportIndicator :: !(Maybe Text),
    -- | "ECOAInquiryType" - Type of inquiry
    consumerCreditReportEquifaxUSConsumerCreditReportEcoaInquiryType :: !(Maybe Text),
    -- | "numberOfMonthsToCountInquiries" - Number of months inquiried
    consumerCreditReportEquifaxUSConsumerCreditReportNumberOfMonthsToCountInquiries :: !(Maybe Text),
    -- | "hitCode"
    consumerCreditReportEquifaxUSConsumerCreditReportHitCode :: !(Maybe ConsumerCreditReportHitCode),
    -- | "fileSinceDate" - Date file was established.  The File Since Date is not intended to represent the date the consumer first became credit active
    consumerCreditReportEquifaxUSConsumerCreditReportFileSinceDate :: !(Maybe Date),
    -- | "lastActivityDate" - Date of Last Activity, (i.e. Header Date of most recent activity in file.)
    consumerCreditReportEquifaxUSConsumerCreditReportLastActivityDate :: !(Maybe Date),
    -- | "reportDate" - Date of this report
    consumerCreditReportEquifaxUSConsumerCreditReportReportDate :: !(Maybe Date),
    -- | "subjectName"
    consumerCreditReportEquifaxUSConsumerCreditReportSubjectName :: !(Maybe ConsumerCreditReportSubjectName),
    -- | "subjectSocialNum" - Subject&#39;s Social Security Number (SSN)
    consumerCreditReportEquifaxUSConsumerCreditReportSubjectSocialNum :: !(Maybe Text),
    -- | "birthDate" - Date of birth of the primary applicant
    consumerCreditReportEquifaxUSConsumerCreditReportBirthDate :: !(Maybe Date),
    -- | "age" - Subject&#39;s age
    consumerCreditReportEquifaxUSConsumerCreditReportAge :: !(Maybe Text),
    -- | "nameMatchFlags"
    consumerCreditReportEquifaxUSConsumerCreditReportNameMatchFlags :: !(Maybe ConsumerCreditReportNameMatchFlags),
    -- | "linkIndicator" - The result of various conditions by the database search using the data submitted in the inquiry, such as   - A file with the same name and address is not found, but the social search finds a file believed to be the subject of the search.    - Results from a marriage or divorce. The consumer&#39;s file has the maiden name and the inquiry has the married name. The same holds true in a divorce case where the consumer reverts to a maiden name    - Results of a consumer using a middle name when applying for credit instead of the first name which is the name that resides on the data base.    - Results of a consumer using an address that does not currently reside on the consumer&#39;s credit file.
    consumerCreditReportEquifaxUSConsumerCreditReportLinkIndicator :: !(Maybe Text),
    -- | "doNotCombineIndicator" - This indicator may appear on the report for various reasons, such as:   - The system encountered a condition where a father/son, brother/brother, or non‐related consumer/consumer files have been mixed by applying a consumer&#39;s data to the wrong file.    - When a fraud indicator (Q, R, T or V) is placed on the file, the Do Not Combine indicator is placed on the file to avoid the file from combining with a file that may have been created as a result of fraud.
    consumerCreditReportEquifaxUSConsumerCreditReportDoNotCombineIndicator :: !(Maybe Text),
    -- | "addressDiscrepancyIndicator" - Indicates if a substantial difference was found between address(es) submitted in the inquiry and the address(es) already existing on a credit file.  The Address Discrepancy Indicator codes that can be returned in the Header element are:   - Y: a substantial difference occurred    - N: no substantial difference occurred
    consumerCreditReportEquifaxUSConsumerCreditReportAddressDiscrepancyIndicator :: !(Maybe Text),
    -- | "fraudSocialNumAlertCode"
    consumerCreditReportEquifaxUSConsumerCreditReportFraudSocialNumAlertCode :: !(Maybe ConsumerCreditReportFraudSocialNumAlertCode),
    -- | "fraudSocialNumAlertCode"
    consumerCreditReportEquifaxUSConsumerCreditReportFraudIDScanAlertCode :: !(Maybe [ConsumerCreditReportFraudIDScanAlertCode]),
    -- | "fraudVictimIndicator"
    consumerCreditReportEquifaxUSConsumerCreditReportFraudVictimIndicator :: !(Maybe ConsumerCreditReportFraudVictimIndicator),
    -- | "addresses" - It contains Equifax report data pertaining to the subject&#39;s addresses
    consumerCreditReportEquifaxUSConsumerCreditReportAddresses :: !(Maybe [Address]),
    -- | "identityScan"
    consumerCreditReportEquifaxUSConsumerCreditReportIdentityScan :: !(Maybe ConsumerCreditReportIdentityScan),
    -- | "formerNames" - It contains any former name of the subject
    consumerCreditReportEquifaxUSConsumerCreditReportFormerNames :: !(Maybe [ConsumerCreditReportFormerNames]),
    -- | "deathDate" - Date in format MMYYYY where MM is the Month and YYYY is the year
    consumerCreditReportEquifaxUSConsumerCreditReportDeathDate :: !(Maybe Text),
    -- | "employments" - It contains information to the subject&#39;s employments
    consumerCreditReportEquifaxUSConsumerCreditReportEmployments :: !(Maybe [ConsumerCreditReportEmployments]),
    -- | "otherIdentification" - It contains information related to other identification
    consumerCreditReportEquifaxUSConsumerCreditReportOtherIdentification :: !(Maybe [ConsumerCreditReportOtherIdentification]),
    -- | "bankruptcies" - Public record information as it pertains to bankruptcy
    consumerCreditReportEquifaxUSConsumerCreditReportBankruptcies :: !(Maybe [Bankruptcy]),
    -- | "collections"
    consumerCreditReportEquifaxUSConsumerCreditReportCollections :: !(Maybe [Collection]),
    -- | "fileIdentificationNumber" - Provides the consumer&#39;s unique file identification number.  The return of the File Identification Number is an optional feature offered by Equifax.  Contact your Equifax Sales Associate for additional information and activation.
    consumerCreditReportEquifaxUSConsumerCreditReportFileIdentificationNumber :: !(Maybe Text),
    -- | "alertContacts" - Contains contact information for the consumer when a Fraud or Active Duty Alert is on the report. If the credit file contains a Consumer Statement, the Alert Contact information will follow the Consumer Statement on file. A Consumer Statement may be returned regardless of a Fraud or Active Duty Alert being on file (such as when a consumer is explaining his/her circumstances for filing bankruptcy). The Consumer Statement will continue to be returned on those consumer reports.
    consumerCreditReportEquifaxUSConsumerCreditReportAlertContacts :: !(Maybe [ConsumerCreditReportAlertContacts]),
    -- | "trades"
    consumerCreditReportEquifaxUSConsumerCreditReportTrades :: !(Maybe [Trade]),
    -- | "inquiries"
    consumerCreditReportEquifaxUSConsumerCreditReportInquiries :: !(Maybe [ConsumerCreditReportInquiries]),
    -- | "consumerStatements" - Show consumer comments about the report information. A maximum of 99 Consumer Statements can be returned
    consumerCreditReportEquifaxUSConsumerCreditReportConsumerStatements :: !(Maybe [ConsumerCreditReportConsumerStatements]),
    -- | "models" - Specific Scoring Model being requested by the customer (optional) Risk-Based Pricing score information, Dodd Frank, MarketMax, Models and ID Score - Risk are optional services.  Please contact your Equifax Sales Associate for additional information and activation of optional services.
    consumerCreditReportEquifaxUSConsumerCreditReportModels :: !(Maybe [ConsumerCreditReportModels]),
    -- | "onlineDirectory" - It is a convenient, value added feature that automatically delivers telephone numbers and upon request, addresses, of all collection, inquiries and trade segment companies shown on the report.  ON-LINE DIRECTORY is an optional product offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation of your desired ON-LINE DIRECTORY option.
    consumerCreditReportEquifaxUSConsumerCreditReportOnlineDirectory :: !(Maybe [ConsumerCreditReportOnlineDirectory]),
    -- | "identification"
    consumerCreditReportEquifaxUSConsumerCreditReportIdentification :: !(Maybe ConsumerCreditReportIdentification),
    -- | "attributes" - Contains attributes associated with customer specific models. A maximum of 99 Data Attribute can be returned.  Please contact your Equifax Sales Associate for additional information.
    consumerCreditReportEquifaxUSConsumerCreditReportAttributes :: !(Maybe [ConsumerCreditReportAttributes1]),
    -- | "onlineGeoCode" - Delivers the folowing codes:   - Geo S M S A Code      - Geo State Code      - Geo County Code      - Geo Census Tract      - Geo Block Group   These codes will be returned for the current address.  Equifax will also return the Property Address Geo Code when it is included in the inquiry.  On Line GEO Code and Micro Vision are optional services offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation.
    consumerCreditReportEquifaxUSConsumerCreditReportOnlineGeoCode :: !(Maybe [ConsumerCreditReportOnlineGeoCode]),
    -- | "OFACAlerts" - It will contain data from the Compliance Data Center (CDC)  Multiple reports on the same consumer are not available with OFAC Alert. Only the first report is returned. OFAC Alert can be only returned with a Consumer Report and when a No‐Hit on the credit file occurs.
    consumerCreditReportEquifaxUSConsumerCreditReportOfacAlerts :: !(Maybe [A.Value]),
    -- | "consumerReferralLocation"
    consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralLocation :: !(Maybe ConsumerCreditReportConsumerReferralLocation),
    -- | "alternateDataSources"
    consumerCreditReportEquifaxUSConsumerCreditReportAlternateDataSources :: !(Maybe ConsumerCreditReportAlternateDataSources)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportEquifaxUSConsumerCreditReport
instance A.FromJSON ConsumerCreditReportEquifaxUSConsumerCreditReport where
  parseJSON = A.withObject "ConsumerCreditReportEquifaxUSConsumerCreditReport" $ \o ->
    ConsumerCreditReportEquifaxUSConsumerCreditReport
      <$> (o .:? "identifier")
      <*> (o .:? "customerReferenceNumber")
      <*> (o .:? "customerNumber")
      <*> (o .:? "consumerReferralCode")
      <*> (o .:? "multipleReportIndicator")
      <*> (o .:? "ECOAInquiryType")
      <*> (o .:? "numberOfMonthsToCountInquiries")
      <*> (o .:? "hitCode")
      <*> (o .:? "fileSinceDate")
      <*> (o .:? "lastActivityDate")
      <*> (o .:? "reportDate")
      <*> (o .:? "subjectName")
      <*> (o .:? "subjectSocialNum")
      <*> (o .:? "birthDate")
      <*> (o .:? "age")
      <*> (o .:? "nameMatchFlags")
      <*> (o .:? "linkIndicator")
      <*> (o .:? "doNotCombineIndicator")
      <*> (o .:? "addressDiscrepancyIndicator")
      <*> (o .:? "fraudSocialNumAlertCode")
      <*> (o .:? "fraudIDScanAlertCodes")
      <*> (o .:? "fraudVictimIndicator")
      <*> (o .:? "addresses")
      <*> (o .:? "identityScan")
      <*> (o .:? "formerNames")
      <*> (o .:? "deathDate")
      <*> (o .:? "employments")
      <*> (o .:? "otherIdentification")
      <*> (o .:? "bankruptcies")
      <*> (o .:? "collections")
      <*> (o .:? "fileIdentificationNumber")
      <*> (o .:? "alertContacts")
      <*> (o .:? "trades")
      <*> (o .:? "inquiries")
      <*> (o .:? "consumerStatements")
      <*> (o .:? "models")
      <*> (o .:? "onlineDirectory")
      <*> (o .:? "identification")
      <*> (o .:? "attributes")
      <*> (o .:? "onlineGeoCode")
      <*> (o .:? "OFACAlerts")
      <*> (o .:? "consumerReferralLocation")
      <*> (o .:? "alternateDataSources")

-- | ToJSON ConsumerCreditReportEquifaxUSConsumerCreditReport
instance A.ToJSON ConsumerCreditReportEquifaxUSConsumerCreditReport where
  toJSON ConsumerCreditReportEquifaxUSConsumerCreditReport {..} =
    _omitNulls
      [ "identifier" .= consumerCreditReportEquifaxUSConsumerCreditReportIdentifier,
        "customerReferenceNumber" .= consumerCreditReportEquifaxUSConsumerCreditReportCustomerReferenceNumber,
        "customerNumber" .= consumerCreditReportEquifaxUSConsumerCreditReportCustomerNumber,
        "consumerReferralCode" .= consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralCode,
        "multipleReportIndicator" .= consumerCreditReportEquifaxUSConsumerCreditReportMultipleReportIndicator,
        "ECOAInquiryType" .= consumerCreditReportEquifaxUSConsumerCreditReportEcoaInquiryType,
        "numberOfMonthsToCountInquiries" .= consumerCreditReportEquifaxUSConsumerCreditReportNumberOfMonthsToCountInquiries,
        "hitCode" .= consumerCreditReportEquifaxUSConsumerCreditReportHitCode,
        "fileSinceDate" .= consumerCreditReportEquifaxUSConsumerCreditReportFileSinceDate,
        "lastActivityDate" .= consumerCreditReportEquifaxUSConsumerCreditReportLastActivityDate,
        "reportDate" .= consumerCreditReportEquifaxUSConsumerCreditReportReportDate,
        "subjectName" .= consumerCreditReportEquifaxUSConsumerCreditReportSubjectName,
        "subjectSocialNum" .= consumerCreditReportEquifaxUSConsumerCreditReportSubjectSocialNum,
        "birthDate" .= consumerCreditReportEquifaxUSConsumerCreditReportBirthDate,
        "age" .= consumerCreditReportEquifaxUSConsumerCreditReportAge,
        "nameMatchFlags" .= consumerCreditReportEquifaxUSConsumerCreditReportNameMatchFlags,
        "linkIndicator" .= consumerCreditReportEquifaxUSConsumerCreditReportLinkIndicator,
        "doNotCombineIndicator" .= consumerCreditReportEquifaxUSConsumerCreditReportDoNotCombineIndicator,
        "addressDiscrepancyIndicator" .= consumerCreditReportEquifaxUSConsumerCreditReportAddressDiscrepancyIndicator,
        "fraudSocialNumAlertCode" .= consumerCreditReportEquifaxUSConsumerCreditReportFraudSocialNumAlertCode,
        "fraudIDScanAlertCode" .= consumerCreditReportEquifaxUSConsumerCreditReportFraudIDScanAlertCode,
        "fraudVictimIndicator" .= consumerCreditReportEquifaxUSConsumerCreditReportFraudVictimIndicator,
        "addresses" .= consumerCreditReportEquifaxUSConsumerCreditReportAddresses,
        "identityScan" .= consumerCreditReportEquifaxUSConsumerCreditReportIdentityScan,
        "formerNames" .= consumerCreditReportEquifaxUSConsumerCreditReportFormerNames,
        "deathDate" .= consumerCreditReportEquifaxUSConsumerCreditReportDeathDate,
        "employments" .= consumerCreditReportEquifaxUSConsumerCreditReportEmployments,
        "otherIdentification" .= consumerCreditReportEquifaxUSConsumerCreditReportOtherIdentification,
        "bankruptcies" .= consumerCreditReportEquifaxUSConsumerCreditReportBankruptcies,
        "collections" .= consumerCreditReportEquifaxUSConsumerCreditReportCollections,
        "fileIdentificationNumber" .= consumerCreditReportEquifaxUSConsumerCreditReportFileIdentificationNumber,
        "alertContacts" .= consumerCreditReportEquifaxUSConsumerCreditReportAlertContacts,
        "trades" .= consumerCreditReportEquifaxUSConsumerCreditReportTrades,
        "inquiries" .= consumerCreditReportEquifaxUSConsumerCreditReportInquiries,
        "consumerStatements" .= consumerCreditReportEquifaxUSConsumerCreditReportConsumerStatements,
        "models" .= consumerCreditReportEquifaxUSConsumerCreditReportModels,
        "onlineDirectory" .= consumerCreditReportEquifaxUSConsumerCreditReportOnlineDirectory,
        "identification" .= consumerCreditReportEquifaxUSConsumerCreditReportIdentification,
        "attributes" .= consumerCreditReportEquifaxUSConsumerCreditReportAttributes,
        "onlineGeoCode" .= consumerCreditReportEquifaxUSConsumerCreditReportOnlineGeoCode,
        "OFACAlerts" .= consumerCreditReportEquifaxUSConsumerCreditReportOfacAlerts,
        "consumerReferralLocation" .= consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralLocation,
        "alternateDataSources" .= consumerCreditReportEquifaxUSConsumerCreditReportAlternateDataSources
      ]

-- | Construct a value of type 'ConsumerCreditReportEquifaxUSConsumerCreditReport' (by applying it's required fields, if any)
mkConsumerCreditReportEquifaxUSConsumerCreditReport ::
  ConsumerCreditReportEquifaxUSConsumerCreditReport
mkConsumerCreditReportEquifaxUSConsumerCreditReport =
  ConsumerCreditReportEquifaxUSConsumerCreditReport
    { consumerCreditReportEquifaxUSConsumerCreditReportIdentifier = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportCustomerReferenceNumber = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportCustomerNumber = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralCode = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportMultipleReportIndicator = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportEcoaInquiryType = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportNumberOfMonthsToCountInquiries = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportHitCode = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportFileSinceDate = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportLastActivityDate = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportReportDate = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportSubjectName = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportSubjectSocialNum = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportBirthDate = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportAge = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportNameMatchFlags = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportLinkIndicator = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportDoNotCombineIndicator = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportAddressDiscrepancyIndicator = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportFraudSocialNumAlertCode = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportFraudIDScanAlertCode = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportFraudVictimIndicator = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportAddresses = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportIdentityScan = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportFormerNames = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportDeathDate = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportEmployments = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportOtherIdentification = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportBankruptcies = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportCollections = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportFileIdentificationNumber = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportAlertContacts = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportTrades = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportInquiries = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportConsumerStatements = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportModels = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportOnlineDirectory = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportIdentification = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportAttributes = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportOnlineGeoCode = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportOfacAlerts = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportConsumerReferralLocation = Nothing,
      consumerCreditReportEquifaxUSConsumerCreditReportAlternateDataSources = Nothing
    }

-- Hit code
data ConsumerCreditReportHitCode = ConsumerCreditReportHitCode
  { -- | "code" - Code value
    consumerCreditReportHitCodeCode :: !(Maybe Text),
    -- | "description" - - 1: Hit - 2: No-Hit - 3: Manual File (Returned if credit report has been designated by Equifax as manual return only or in response to an on-line prescreen transaction when the file contains a promo block.) - 4: Manual File Review Required - Due to reasons such as unable to deliver Consumer Narrative(s), Fraud/Active Duty Alert(s), or Address Discrepancy Indicator - 5: Referred File (File is under review or file is being referred to another) - 6: Hit and Automated Consumer Narrative - 7: Fraud/Verification Product Being Requested Without Requesting a Credit File - 8: \&quot;Thin File\&quot; when using Fraud/Verification Products - 9: No-Hit/Auto-DTEC** - A: Consumer Requested Security Freeze on His/Her Credit File Report Unavailable (See Attachment 15 Legislative Information) - C: No-Hit With Information From Additional Data Source(s) Returned - D: Manual File With Information From Additional Data Source(s) Returned - E: Manual Consumer Narrative With Information From Additional Data Source(s) Returned - F: Referred File With Information from Additional Data Source(s) Returned - G: Consumer Requested Security Freeze on His/Her Credit File – Report Unavailable With Information From Additional Data Source(s) Returned (except MarketMax)  - I*: Information from your inquiry has been identified as potentially fraudulent or misused, therefore the credit report is not available for delivery  - J*: Information from your inquiry has been identified as potentially fraudulent or misused, therefore the credit report is not available for delivery – with information from additional data source(s) returned - L*: Consumer Requested Equifax Lock on His/Her Credit File – Report Unavailable - M*: Consumer Requested Equifax Lock on His/Her Credit File – Report Unavailable With Information From Additional Data Source(s) Returned - T: Additional data source being returned without requesting a consumer report  - *:Optional Hit Codes require your customer number to be activated through your Sales Representative.
    consumerCreditReportHitCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportHitCode
instance A.FromJSON ConsumerCreditReportHitCode where
  parseJSON = A.withObject "ConsumerCreditReportHitCode" $ \o ->
    ConsumerCreditReportHitCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportHitCode
instance A.ToJSON ConsumerCreditReportHitCode where
  toJSON ConsumerCreditReportHitCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportHitCodeCode,
        "description" .= consumerCreditReportHitCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportHitCode' (by applying it's required fields, if any)
mkConsumerCreditReportHitCode ::
  ConsumerCreditReportHitCode
mkConsumerCreditReportHitCode =
  ConsumerCreditReportHitCode
    { consumerCreditReportHitCodeCode = Nothing,
      consumerCreditReportHitCodeDescription = Nothing
    }

--

-- ** ConsumerCreditReportSubjectName

-- | ConsumerCreditReportSubjectName
-- Subject's full name
data ConsumerCreditReportSubjectName = ConsumerCreditReportSubjectName
  { -- | "firstName" - Subject&#39;s first name
    consumerCreditReportSubjectNameFirstName :: !(Maybe Text),
    -- | "lastName" - Subject&#39;s last name
    consumerCreditReportSubjectNameLastName :: !(Maybe Text),
    -- | "middleName" - Subject&#39;s middle name
    consumerCreditReportSubjectNameMiddleName :: !(Maybe Text),
    -- | "suffix" - Subject&#39;s suffix name
    consumerCreditReportSubjectNameSuffix :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportSubjectName
instance A.FromJSON ConsumerCreditReportSubjectName where
  parseJSON = A.withObject "ConsumerCreditReportSubjectName" $ \o ->
    ConsumerCreditReportSubjectName
      <$> (o .:? "firstName")
      <*> (o .:? "lastName")
      <*> (o .:? "middleName")
      <*> (o .:? "suffix")

-- | ToJSON ConsumerCreditReportSubjectName
instance A.ToJSON ConsumerCreditReportSubjectName where
  toJSON ConsumerCreditReportSubjectName {..} =
    _omitNulls
      [ "firstName" .= consumerCreditReportSubjectNameFirstName,
        "lastName" .= consumerCreditReportSubjectNameLastName,
        "middleName" .= consumerCreditReportSubjectNameMiddleName,
        "suffix" .= consumerCreditReportSubjectNameSuffix
      ]

-- | Construct a value of type 'ConsumerCreditReportSubjectName' (by applying it's required fields, if any)
mkConsumerCreditReportSubjectName ::
  ConsumerCreditReportSubjectName
mkConsumerCreditReportSubjectName =
  ConsumerCreditReportSubjectName
    { consumerCreditReportSubjectNameFirstName = Nothing,
      consumerCreditReportSubjectNameLastName = Nothing,
      consumerCreditReportSubjectNameMiddleName = Nothing,
      consumerCreditReportSubjectNameSuffix = Nothing
    }

-- ** E'FirstNameMatchFlag

-- | Enum of 'Text' .
-- Flag indicates if the subject's first name matches
data E'FirstNameMatchFlag
  = -- | @"Y"@
    E'FirstNameMatchFlag'Y
  | -- | @"N"@
    E'FirstNameMatchFlag'N
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON E'FirstNameMatchFlag where toJSON = A.toJSON . fromE'FirstNameMatchFlag

instance A.FromJSON E'FirstNameMatchFlag where parseJSON o = P.either P.fail (pure . P.id) . toE'FirstNameMatchFlag =<< A.parseJSON o

instance WH.ToHttpApiData E'FirstNameMatchFlag where toQueryParam = WH.toQueryParam . fromE'FirstNameMatchFlag

instance WH.FromHttpApiData E'FirstNameMatchFlag where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toE'FirstNameMatchFlag

instance MimeRender MimeMultipartFormData E'FirstNameMatchFlag where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'E'FirstNameMatchFlag' enum
fromE'FirstNameMatchFlag :: E'FirstNameMatchFlag -> Text
fromE'FirstNameMatchFlag = \case
  E'FirstNameMatchFlag'Y -> "Y"
  E'FirstNameMatchFlag'N -> "N"

-- | parse 'E'FirstNameMatchFlag' enum
toE'FirstNameMatchFlag :: Text -> P.Either String E'FirstNameMatchFlag
toE'FirstNameMatchFlag = \case
  "Y" -> P.Right E'FirstNameMatchFlag'Y
  "N" -> P.Right E'FirstNameMatchFlag'N
  s -> P.Left $ "toE'FirstNameMatchFlag: enum parse failure: " P.++ P.show s

-- ** ConsumerCreditReportNameMatchFlags

-- | ConsumerCreditReportNameMatchFlags
-- Flags indicate if the subject's name matches
data ConsumerCreditReportNameMatchFlags = ConsumerCreditReportNameMatchFlags
  { -- | "firstNameMatchFlag" - Flag indicates if the subject&#39;s first name matches
    consumerCreditReportNameMatchFlagsFirstNameMatchFlag :: !(Maybe E'FirstNameMatchFlag),
    -- | "lastNameMatchFlag" - Flag indicates if the subject&#39;s last name matches
    consumerCreditReportNameMatchFlagsLastNameMatchFlag :: !(Maybe E'FirstNameMatchFlag),
    -- | "middleNameMatchFlag" - Flag indicates if the subject&#39;s middle name matches
    consumerCreditReportNameMatchFlagsMiddleNameMatchFlag :: !(Maybe E'FirstNameMatchFlag),
    -- | "suffixMatchFlag" - Flag indicates if the subject&#39;s suffix name matches
    consumerCreditReportNameMatchFlagsSuffixMatchFlag :: !(Maybe E'FirstNameMatchFlag)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportNameMatchFlags
instance A.FromJSON ConsumerCreditReportNameMatchFlags where
  parseJSON = A.withObject "ConsumerCreditReportNameMatchFlags" $ \o ->
    ConsumerCreditReportNameMatchFlags
      <$> (o .:? "firstNameMatchFlag")
      <*> (o .:? "lastNameMatchFlag")
      <*> (o .:? "middleNameMatchFlag")
      <*> (o .:? "suffixMatchFlag")

-- | ToJSON ConsumerCreditReportNameMatchFlags
instance A.ToJSON ConsumerCreditReportNameMatchFlags where
  toJSON ConsumerCreditReportNameMatchFlags {..} =
    _omitNulls
      [ "firstNameMatchFlag" .= consumerCreditReportNameMatchFlagsFirstNameMatchFlag,
        "lastNameMatchFlag" .= consumerCreditReportNameMatchFlagsLastNameMatchFlag,
        "middleNameMatchFlag" .= consumerCreditReportNameMatchFlagsMiddleNameMatchFlag,
        "suffixMatchFlag" .= consumerCreditReportNameMatchFlagsSuffixMatchFlag
      ]

-- | Construct a value of type 'ConsumerCreditReportNameMatchFlags' (by applying it's required fields, if any)
mkConsumerCreditReportNameMatchFlags ::
  ConsumerCreditReportNameMatchFlags
mkConsumerCreditReportNameMatchFlags =
  ConsumerCreditReportNameMatchFlags
    { consumerCreditReportNameMatchFlagsFirstNameMatchFlag = Nothing,
      consumerCreditReportNameMatchFlagsLastNameMatchFlag = Nothing,
      consumerCreditReportNameMatchFlagsMiddleNameMatchFlag = Nothing,
      consumerCreditReportNameMatchFlagsSuffixMatchFlag = Nothing
    }

-- ** ConsumerCreditReportFraudSocialNumAlertCode

-- | ConsumerCreditReportFraudSocialNumAlertCode
-- FraudIQ™ SSN Alert compares the inquiry social security number and returns a flag to alert customers
data ConsumerCreditReportFraudSocialNumAlertCode = ConsumerCreditReportFraudSocialNumAlertCode
  { -- | "code" - Code value
    consumerCreditReportFraudSocialNumAlertCodeCode :: !(Maybe Text),
    -- | "description" - FLAG FLAG VERBIAGE DESCRIPTION - A: Inquiry SSN is associated with another consumer - SSN Mismatch   The social security number provided in the inquiry matches   to another consumer on the Equifax database. A credit file   is returned for the consumer name and address provided,   however the social security number on that file is different   from the social security number provided on the inquiry.    - B: Inquiry SSN not present    Inquiry did not contain a social security number,    therefore no comparison can be made.      - N: No Alert available   The social security number provided in the inquiry    cannot be matched to a specific consumer based   on the Equifax proprietary comparison algorithm.    - P: Inquiry SSN is associated with the consumer   The social security number provided in the inquiry   matches to the requested consumer.    - V*: SSN affirm variation    Inquiry ssn has a slight variation with consumer.    The social security number provided in the inquiry   has a slight variation to the requested consumer.    - W: Inquiry SSN is associated with another consumer   The social security number provided in the inquiry   matches to another consumer in the Equifax database.    No credit file is available for the requested consumer.    - *: Optional code – requires that your Equifax Customer Number be activated to   be returned when encountered.
    consumerCreditReportFraudSocialNumAlertCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportFraudSocialNumAlertCode
instance A.FromJSON ConsumerCreditReportFraudSocialNumAlertCode where
  parseJSON = A.withObject "ConsumerCreditReportFraudSocialNumAlertCode" $ \o ->
    ConsumerCreditReportFraudSocialNumAlertCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportFraudSocialNumAlertCode
instance A.ToJSON ConsumerCreditReportFraudSocialNumAlertCode where
  toJSON ConsumerCreditReportFraudSocialNumAlertCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportFraudSocialNumAlertCodeCode,
        "description" .= consumerCreditReportFraudSocialNumAlertCodeDescription
      ]

data ConsumerCreditReportFraudIDScanAlertCode = ConsumerCreditReportFraudIDScanAlertCode
  { -- | "code" - Code value
    consumerCreditReportFraudIDScanAlertCodeCode :: !(Maybe Text),
    -- | "description" - FLAG FLAG VERBIAGE DESCRIPTION - A: Inquiry SSN is associated with another consumer - SSN Mismatch   The social security number provided in the inquiry matches   to another consumer on the Equifax database. A credit file   is returned for the consumer name and address provided,   however the social security number on that file is different   from the social security number provided on the inquiry.    - B: Inquiry SSN not present    Inquiry did not contain a social security number,    therefore no comparison can be made.      - N: No Alert available   The social security number provided in the inquiry    cannot be matched to a specific consumer based   on the Equifax proprietary comparison algorithm.    - P: Inquiry SSN is associated with the consumer   The social security number provided in the inquiry   matches to the requested consumer.    - V*: SSN affirm variation    Inquiry ssn has a slight variation with consumer.    The social security number provided in the inquiry   has a slight variation to the requested consumer.    - W: Inquiry SSN is associated with another consumer   The social security number provided in the inquiry   matches to another consumer in the Equifax database.    No credit file is available for the requested consumer.    - *: Optional code – requires that your Equifax Customer Number be activated to   be returned when encountered.
    consumerCreditReportFraudIDScanAlertCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

instance A.FromJSON ConsumerCreditReportFraudIDScanAlertCode where
  parseJSON = A.withObject "ConsumerCreditReportFraudIDScanAlertCode" $ \o ->
    ConsumerCreditReportFraudIDScanAlertCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportFraudIDScanAlertCodes
instance A.ToJSON ConsumerCreditReportFraudIDScanAlertCode where
  toJSON ConsumerCreditReportFraudIDScanAlertCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportFraudIDScanAlertCodeCode,
        "description" .= consumerCreditReportFraudIDScanAlertCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportFraudSocialNumAlertCode' (by applying it's required fields, if any)
mkConsumerCreditReportFraudSocialNumAlertCode ::
  ConsumerCreditReportFraudSocialNumAlertCode
mkConsumerCreditReportFraudSocialNumAlertCode =
  ConsumerCreditReportFraudSocialNumAlertCode
    { consumerCreditReportFraudSocialNumAlertCodeCode = Nothing,
      consumerCreditReportFraudSocialNumAlertCodeDescription = Nothing
    }

-- ** ConsumerCreditReportFraudVictimIndicator

-- | ConsumerCreditReportFraudVictimIndicator
-- Indicates fraud victim alert type
data ConsumerCreditReportFraudVictimIndicator = ConsumerCreditReportFraudVictimIndicator
  { -- | "code" - Code value
    consumerCreditReportFraudVictimIndicatorCode :: !(Maybe Text),
    -- | "description" - The following indicator codes can be returned in the Header segment:    - N: Active Duty Alert    - Q: Active Duty Alert with Fraud Victim “Initial Alert”    - R: Active Duty Alert with Fraud Victim T (police report)    - T: Fraud Victim (associated with trade)    - V: Fraud Victim “Initial Alert”    - W: Active Duty Alert with Fraud Victim “Extended Alert”    - X: Fraud Victim “Extended Alert”  Consumers or their representatives may request that an Initial Fraud Alert or, if the consumer is a member of the military on active duty, an Active Duty Alert be placed on their credit file. The Initial Fraud Alert will remain on the credit file for one year (366 days); the Active Duty Alert will remain on file for twelve months. Equifax will return an “N”, “Q”, or “V”, as applicable, in the Header segment. A user receiving a report containing an Initial Fraud Alert or Active Duty Alert may not grant credit without first determining the identity of the person making the request. If the consumer has provided a telephone number, it will be returned in a second Consumer Narrative Statement segment or Alert Contact segment and must be used for identity verification purposes before granting credit.
    consumerCreditReportFraudVictimIndicatorDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportFraudVictimIndicator
instance A.FromJSON ConsumerCreditReportFraudVictimIndicator where
  parseJSON = A.withObject "ConsumerCreditReportFraudVictimIndicator" $ \o ->
    ConsumerCreditReportFraudVictimIndicator
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportFraudVictimIndicator
instance A.ToJSON ConsumerCreditReportFraudVictimIndicator where
  toJSON ConsumerCreditReportFraudVictimIndicator {..} =
    _omitNulls
      [ "code" .= consumerCreditReportFraudVictimIndicatorCode,
        "description" .= consumerCreditReportFraudVictimIndicatorDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportFraudVictimIndicator' (by applying it's required fields, if any)
mkConsumerCreditReportFraudVictimIndicator ::
  ConsumerCreditReportFraudVictimIndicator
mkConsumerCreditReportFraudVictimIndicator =
  ConsumerCreditReportFraudVictimIndicator
    { consumerCreditReportFraudVictimIndicatorCode = Nothing,
      consumerCreditReportFraudVictimIndicatorDescription = Nothing
    }

-- ** ConsumerCreditReportIdentityScan

-- | ConsumerCreditReportIdentityScan
-- It contains the FraudIQ Identity Scan Alert information.  FraudIQ Identity Scan Alert is an optional product offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation of your Identity Scan Alert option.
data ConsumerCreditReportIdentityScan = ConsumerCreditReportIdentityScan
  { -- | "alertCodes" - It contains up to 15 Identity Scan Alert codes
    consumerCreditReportIdentityScanAlertCodes :: !(Maybe [ConsumerCreditReportIdentityScanAlertCodes]),
    -- | "identityScanRegulated" - byte that distinguished ID Scan is Non-regulated
    consumerCreditReportIdentityScanIdentityScanRegulated :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportIdentityScan
instance A.FromJSON ConsumerCreditReportIdentityScan where
  parseJSON = A.withObject "ConsumerCreditReportIdentityScan" $ \o ->
    ConsumerCreditReportIdentityScan
      <$> (o .:? "alertCodes")
      <*> (o .:? "identityScanRegulated")

-- | ToJSON ConsumerCreditReportIdentityScan
instance A.ToJSON ConsumerCreditReportIdentityScan where
  toJSON ConsumerCreditReportIdentityScan {..} =
    _omitNulls
      [ "alertCodes" .= consumerCreditReportIdentityScanAlertCodes,
        "identityScanRegulated" .= consumerCreditReportIdentityScanIdentityScanRegulated
      ]

-- | Construct a value of type 'ConsumerCreditReportIdentityScan' (by applying it's required fields, if any)
mkConsumerCreditReportIdentityScan ::
  ConsumerCreditReportIdentityScan
mkConsumerCreditReportIdentityScan =
  ConsumerCreditReportIdentityScan
    { consumerCreditReportIdentityScanAlertCodes = Nothing,
      consumerCreditReportIdentityScanIdentityScanRegulated = Nothing
    }

--

-- ** ConsumerCreditReportFormerNames

-- | ConsumerCreditReportFormerNames
data ConsumerCreditReportFormerNames = ConsumerCreditReportFormerNames
  { -- | "lastName" - Subject&#39;s former last name
    consumerCreditReportFormerNamesLastName :: !(Maybe Text),
    -- | "firstName" - Subject&#39;s former first name
    consumerCreditReportFormerNamesFirstName :: !(Maybe Text),
    -- | "middleInitial" - Subject&#39;s former middle name
    consumerCreditReportFormerNamesMiddleInitial :: !(Maybe Text),
    -- | "suffix" - Subject&#39;s former suffix
    consumerCreditReportFormerNamesSuffix :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportFormerNames
instance A.FromJSON ConsumerCreditReportFormerNames where
  parseJSON = A.withObject "ConsumerCreditReportFormerNames" $ \o ->
    ConsumerCreditReportFormerNames
      <$> (o .:? "lastName")
      <*> (o .:? "firstName")
      <*> (o .:? "middleInitial")
      <*> (o .:? "suffix")

-- | ToJSON ConsumerCreditReportFormerNames
instance A.ToJSON ConsumerCreditReportFormerNames where
  toJSON ConsumerCreditReportFormerNames {..} =
    _omitNulls
      [ "lastName" .= consumerCreditReportFormerNamesLastName,
        "firstName" .= consumerCreditReportFormerNamesFirstName,
        "middleInitial" .= consumerCreditReportFormerNamesMiddleInitial,
        "suffix" .= consumerCreditReportFormerNamesSuffix
      ]

-- | Construct a value of type 'ConsumerCreditReportFormerNames' (by applying it's required fields, if any)
mkConsumerCreditReportFormerNames ::
  ConsumerCreditReportFormerNames
mkConsumerCreditReportFormerNames =
  ConsumerCreditReportFormerNames
    { consumerCreditReportFormerNamesLastName = Nothing,
      consumerCreditReportFormerNamesFirstName = Nothing,
      consumerCreditReportFormerNamesMiddleInitial = Nothing,
      consumerCreditReportFormerNamesSuffix = Nothing
    }

--

-- ** ConsumerCreditReportEmployments

-- | ConsumerCreditReportEmployments
data ConsumerCreditReportEmployments = ConsumerCreditReportEmployments
  { -- | "identifier" - It describes the type of employment
    consumerCreditReportEmploymentsIdentifier :: !(Maybe Text),
    -- | "occupation" - Subject&#39;s occupation
    consumerCreditReportEmploymentsOccupation :: !(Maybe Text),
    -- | "employer" - Employer&#39;s name
    consumerCreditReportEmploymentsEmployer :: !(Maybe Text),
    -- | "dateLastReported" - Date in format MMYYYY where MM is the Month and YYYY is the year
    consumerCreditReportEmploymentsDateLastReported :: !(Maybe Text),
    -- | "dateFirstReported" - Date in format MMYYYY where MM is the Month and YYYY is the year
    consumerCreditReportEmploymentsDateFirstReported :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportEmployments
instance A.FromJSON ConsumerCreditReportEmployments where
  parseJSON = A.withObject "ConsumerCreditReportEmployments" $ \o ->
    ConsumerCreditReportEmployments
      <$> (o .:? "identifier")
      <*> (o .:? "occupation")
      <*> (o .:? "employer")
      <*> (o .:? "dateLastReported")
      <*> (o .:? "dateFirstReported")

-- | ToJSON ConsumerCreditReportEmployments
instance A.ToJSON ConsumerCreditReportEmployments where
  toJSON ConsumerCreditReportEmployments {..} =
    _omitNulls
      [ "identifier" .= consumerCreditReportEmploymentsIdentifier,
        "occupation" .= consumerCreditReportEmploymentsOccupation,
        "employer" .= consumerCreditReportEmploymentsEmployer,
        "dateLastReported" .= consumerCreditReportEmploymentsDateLastReported,
        "dateFirstReported" .= consumerCreditReportEmploymentsDateFirstReported
      ]

-- | Construct a value of type 'ConsumerCreditReportEmployments' (by applying it's required fields, if any)
mkConsumerCreditReportEmployments ::
  ConsumerCreditReportEmployments
mkConsumerCreditReportEmployments =
  ConsumerCreditReportEmployments
    { consumerCreditReportEmploymentsIdentifier = Nothing,
      consumerCreditReportEmploymentsOccupation = Nothing,
      consumerCreditReportEmploymentsEmployer = Nothing,
      consumerCreditReportEmploymentsDateLastReported = Nothing,
      consumerCreditReportEmploymentsDateFirstReported = Nothing
    }

-- ** ConsumerCreditReportOtherIdentification

-- | ConsumerCreditReportOtherIdentification
data ConsumerCreditReportOtherIdentification = ConsumerCreditReportOtherIdentification
  { -- | "dateReported" - Date the other identification was reported to Equifax
    consumerCreditReportOtherIdentificationDateReported :: !(Maybe Date),
    -- | "typeCode"
    consumerCreditReportOtherIdentificationTypeCode :: !(Maybe ConsumerCreditReportTypeCode),
    -- | "identificationNumber" - Identification number
    consumerCreditReportOtherIdentificationIdentificationNumber :: !(Maybe Text),
    -- | "reasonCode"
    consumerCreditReportOtherIdentificationReasonCode :: !(Maybe ConsumerCreditReportReasonCode)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportOtherIdentification
instance A.FromJSON ConsumerCreditReportOtherIdentification where
  parseJSON = A.withObject "ConsumerCreditReportOtherIdentification" $ \o ->
    ConsumerCreditReportOtherIdentification
      <$> (o .:? "dateReported")
      <*> (o .:? "typeCode")
      <*> (o .:? "identificationNumber")
      <*> (o .:? "reasonCode")

-- | ToJSON ConsumerCreditReportOtherIdentification
instance A.ToJSON ConsumerCreditReportOtherIdentification where
  toJSON ConsumerCreditReportOtherIdentification {..} =
    _omitNulls
      [ "dateReported" .= consumerCreditReportOtherIdentificationDateReported,
        "typeCode" .= consumerCreditReportOtherIdentificationTypeCode,
        "identificationNumber" .= consumerCreditReportOtherIdentificationIdentificationNumber,
        "reasonCode" .= consumerCreditReportOtherIdentificationReasonCode
      ]

-- | Construct a value of type 'ConsumerCreditReportOtherIdentification' (by applying it's required fields, if any)
mkConsumerCreditReportOtherIdentification ::
  ConsumerCreditReportOtherIdentification
mkConsumerCreditReportOtherIdentification =
  ConsumerCreditReportOtherIdentification
    { consumerCreditReportOtherIdentificationDateReported = Nothing,
      consumerCreditReportOtherIdentificationTypeCode = Nothing,
      consumerCreditReportOtherIdentificationIdentificationNumber = Nothing,
      consumerCreditReportOtherIdentificationReasonCode = Nothing
    }

-- ** ConsumerCreditReportIdentityScanAlertCodes

-- | ConsumerCreditReportIdentityScanAlertCodes
-- Describes the Identity Scan Alerts
data ConsumerCreditReportIdentityScanAlertCodes = ConsumerCreditReportIdentityScanAlertCodes
  { -- | "code" - Code value
    consumerCreditReportIdentityScanAlertCodesCode :: !(Maybe Text),
    -- | "description" - Description for the given code   - A: INQUIRY SSN HAS NEVER BEEN ISSUED OR WAS ISSUED AFTER JUNE 2011   - B: INQUIRY SSN REPORTED AS MISUSED   - C: INQUIRY ADDRESS ASSOCIATED WITH MORE THAN ONE NAME OR SSN   - D: INQUIRY ADDRESS UNVERIFIABLE   - G: INQUIRY ADDRESS IS LISTED AS A U.S. POST OFFICE STREET ADDRESS   - H: INQUIRY ADDRESS IS LISTED AS A CAMPGROUND   - I: INQUIRY SSN ASSOCIATED WITH PERSON REPORTED AS DECEASED   - J: INQUIRY ADDRESS IS LISTED AS A HOTEL/MOTEL   - L: FRAUD VICTIM ALERT PRESENT IN DATABASE   - M: INQUIRY SSN ISSUED RECENTLY   - N: ACTIVE DUTY ALERT PRESENT IN DATABASE   - O: INQUIRY SSN ISSUED PRIOR TO INQUIRY DATE OF BIRTH   - P: INQUIRY ADDRESS IS LISTED AS A CORRECTIONAL INSTITUTION ADDRESS   - Q: INQUIRY SSN REPORTED AS DECEASED AND LAST NAME DOES NOT MATCH   - R: INQUIRY ADDRESS IS NOT ASSOCIATED WITH THIS CONSUMER NAME   - S: IDENTITY SCAN DID NOT DETECT ANY ALERTS   - W: INQUIRY ADDRESS IS LISTED AS A NON-RESIDENTIAL ADDRESS   - X: INQUIRY ADDRESS ASSOCIATED WITH REPORTED FRAUD   - Y: INQUIRY TELEPHONE NUMBER LISTED AS A COMMERCIAL PHONE   - Z: INQUIRY ADDRESS IS LISTED AS A MAIL RECEIVING SERVICE   - 0: INCOMPLETE SCAN - LIMITED DATA SOURCES AVAILABLE   - 1: INQUIRY ADDRESS IS LISTED AS A MULTI-DWELLING UNIT   - 2: INQUIRY ADDRESS HAS BEEN REPORTED AS MISUSED   - 4: INQUIRY SSN MAY BE A TAX ID NUMBER   - 5: INQUIRY TELEPHONE NUMBER MAY BELONG TO A MOBILE PHONE   - 7: UNABLE TO PERFORM SSN VALIDATION DUE TO INSUFFICIENT SSN INPUT   - 8: UNABLE TO PERFORM TELEPHONE VALIDATION DUE TO INSUFFICIENT TELEPHONE INPUT   - 9: INQUIRY SSN IS INVALID
    consumerCreditReportIdentityScanAlertCodesDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportIdentityScanAlertCodes
instance A.FromJSON ConsumerCreditReportIdentityScanAlertCodes where
  parseJSON = A.withObject "ConsumerCreditReportIdentityScanAlertCodes" $ \o ->
    ConsumerCreditReportIdentityScanAlertCodes
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportIdentityScanAlertCodes
instance A.ToJSON ConsumerCreditReportIdentityScanAlertCodes where
  toJSON ConsumerCreditReportIdentityScanAlertCodes {..} =
    _omitNulls
      [ "code" .= consumerCreditReportIdentityScanAlertCodesCode,
        "description" .= consumerCreditReportIdentityScanAlertCodesDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportIdentityScanAlertCodes' (by applying it's required fields, if any)
mkConsumerCreditReportIdentityScanAlertCodes ::
  ConsumerCreditReportIdentityScanAlertCodes
mkConsumerCreditReportIdentityScanAlertCodes =
  ConsumerCreditReportIdentityScanAlertCodes
    { consumerCreditReportIdentityScanAlertCodesCode = Nothing,
      consumerCreditReportIdentityScanAlertCodesDescription = Nothing
    }

-- ** Bankruptcy

-- | Bankruptcy
-- Details associated with bankruptcy
data Bankruptcy = Bankruptcy
  { -- | "customerNumber" - Court customer number
    bankruptcyCustomerNumber :: !(Maybe Text),
    -- | "type" - Type of bankruptcy: - B: Business - I: Individual
    bankruptcyType :: !(Maybe Text),
    -- | "filer" - How filed: - I: Individual - J: Joint - W: Spouse
    bankruptcyFiler :: !(Maybe Text),
    -- | "industryCode" - Industry code
    bankruptcyIndustryCode :: !(Maybe Text),
    -- | "currentIntentOrDispositionCode"
    bankruptcyCurrentIntentOrDispositionCode :: !(Maybe BankruptcyCurrentIntentOrDispositionCode),
    -- | "narrativeCodes"
    bankruptcyNarrativeCodes :: !(Maybe [A.Value]),
    -- | "rawNarrativeCodes" - Raw narrative codes included in the report
    bankruptcyRawNarrativeCodes :: !(Maybe [Text]),
    -- | "caseNumber" - Case number
    bankruptcyCaseNumber :: !(Maybe Text),
    -- | "dispositionDate" - Disposition Date
    bankruptcyDispositionDate :: !(Maybe Text),
    -- | "dateFiled" - Date Last Filed
    bankruptcyDateFiled :: !(Maybe Date),
    -- | "currentDispositionDate" - Contains the date of the final status of the case whether discharged or dismissed
    bankruptcyCurrentDispositionDate :: !(Maybe Date),
    -- | "verifiedDate" - Date of verification for any updates to the bankruptcy disposition according to court records.
    bankruptcyVerifiedDate :: !(Maybe Date),
    -- | "priorIntentOrDispositionCode"
    bankruptcyPriorIntentOrDispositionCode :: !(Maybe BankruptcyPriorIntentOrDispositionCode),
    -- | "dateReported" - Date the bankruptcy was reported to Equifax
    bankruptcyDateReported :: !(Maybe Date)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Bankruptcy
instance A.FromJSON Bankruptcy where
  parseJSON = A.withObject "Bankruptcy" $ \o ->
    Bankruptcy
      <$> (o .:? "customerNumber")
      <*> (o .:? "type")
      <*> (o .:? "filer")
      <*> (o .:? "industryCode")
      <*> (o .:? "currentIntentOrDispositionCode")
      <*> (o .:? "narrativeCodes")
      <*> (o .:? "rawNarrativeCodes")
      <*> (o .:? "caseNumber")
      <*> (o .:? "dispositionDate")
      <*> (o .:? "dateFiled")
      <*> (o .:? "currentDispositionDate")
      <*> (o .:? "verifiedDate")
      <*> (o .:? "priorIntentOrDispositionCode")
      <*> (o .:? "dateReported")

-- | ToJSON Bankruptcy
instance A.ToJSON Bankruptcy where
  toJSON Bankruptcy {..} =
    _omitNulls
      [ "customerNumber" .= bankruptcyCustomerNumber,
        "type" .= bankruptcyType,
        "filer" .= bankruptcyFiler,
        "industryCode" .= bankruptcyIndustryCode,
        "currentIntentOrDispositionCode" .= bankruptcyCurrentIntentOrDispositionCode,
        "narrativeCodes" .= bankruptcyNarrativeCodes,
        "rawNarrativeCodes" .= bankruptcyRawNarrativeCodes,
        "caseNumber" .= bankruptcyCaseNumber,
        "dispositionDate" .= bankruptcyDispositionDate,
        "dateFiled" .= bankruptcyDateFiled,
        "currentDispositionDate" .= bankruptcyCurrentDispositionDate,
        "verifiedDate" .= bankruptcyVerifiedDate,
        "priorIntentOrDispositionCode" .= bankruptcyPriorIntentOrDispositionCode,
        "dateReported" .= bankruptcyDateReported
      ]

-- | Construct a value of type 'Bankruptcy' (by applying it's required fields, if any)
mkBankruptcy ::
  Bankruptcy
mkBankruptcy =
  Bankruptcy
    { bankruptcyCustomerNumber = Nothing,
      bankruptcyType = Nothing,
      bankruptcyFiler = Nothing,
      bankruptcyIndustryCode = Nothing,
      bankruptcyCurrentIntentOrDispositionCode = Nothing,
      bankruptcyNarrativeCodes = Nothing,
      bankruptcyRawNarrativeCodes = Nothing,
      bankruptcyCaseNumber = Nothing,
      bankruptcyDispositionDate = Nothing,
      bankruptcyDateFiled = Nothing,
      bankruptcyCurrentDispositionDate = Nothing,
      bankruptcyVerifiedDate = Nothing,
      bankruptcyPriorIntentOrDispositionCode = Nothing,
      bankruptcyDateReported = Nothing
    }

-- ** BankruptcyCurrentIntentOrDispositionCode

-- | BankruptcyCurrentIntentOrDispositionCode
-- Indicates the chapter type and the current status of the bankruptcy
data BankruptcyCurrentIntentOrDispositionCode = BankruptcyCurrentIntentOrDispositionCode
  { -- | "code" - Code value
    bankruptcyCurrentIntentOrDispositionCodeCode :: !(Maybe Text),
    -- | "description" - Chapter types and current status: - A: DISCHARGED CH-7 The amount owed was included in the “order of relief.” Debtor no longer liable for debts listed in “order of relief.” - C: CH-13 FILED Adjustment of debts of an individual with consistent income. Debtor petitions the court for permission to pay a percent of his income over a period of years until debt is satisfied (usually not over three years). - D: CH-11 FILED Business Reorganization. Business debtor is granted relief from payment under terms of initial contract reorganization period. - E: DISMSD/CLSD CH11 Petition for reorganization of debt has been withdrawn by debtor or honored - subject liable for debts. - F: DISCHARGED CH-11 Petition by debtor for complete relief of all debts is honored. Debtor no longer liable for debts listed in “order of relief.” - G: CH-12 FILED The Chapter 12 plan applies only to family farmers who have regular annual incomes sufficient to make payments under a proposed plan. Payment under the plan must be completed in three years. In certain situations, payment of no longer than five years is permitted. - H: DISCHARGED CH-12 A discharge is entered after the completion of all payments under the plan. However, certain payments on some long term claims, which are due after the last payment under the plan, will continue after the date of discharge. - I: INVOLUNTARY CH-7 The debtor is forced into bankruptcy by the petition of a sufficient number of his creditors. - J: DISMSD/CLSD CH12 The court may dismiss the plan or terminate the plan for various reasons, including unreasonable delays, gross mismanagement, non payment of any fees and charges, failure to file a plan in a timely manner, failure to make timely payments required by a confirmed plan, denial of confirmation or request made for additional time, and so forth. - K: DISMSD/CLSD CH13 The petition by debtor for permission to pay a percent of his income over a period of years has been withdrawn by debtor or has not been followed by debtor. Debtor remains liable for his debts under initial terms of the contract. - L: DISCHARGED CH-13 Plan by debtor to pay percent of income over a period of years has been completed. Debtor no longer liable for debts listed in payment plan. - M: DISMSD/CLSD CH7 The bankruptcy petition has been withdrawn by or has not been honored by the court. Subject remains liable for his debts under the terms of the initial contract. - V: VOLUNTARY CH-7 Bankruptcy proceeding is initiated by the debtor&#39;s own petition to be declared bankrupt and have benefit of the law.
    bankruptcyCurrentIntentOrDispositionCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON BankruptcyCurrentIntentOrDispositionCode
instance A.FromJSON BankruptcyCurrentIntentOrDispositionCode where
  parseJSON = A.withObject "BankruptcyCurrentIntentOrDispositionCode" $ \o ->
    BankruptcyCurrentIntentOrDispositionCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON BankruptcyCurrentIntentOrDispositionCode
instance A.ToJSON BankruptcyCurrentIntentOrDispositionCode where
  toJSON BankruptcyCurrentIntentOrDispositionCode {..} =
    _omitNulls
      [ "code" .= bankruptcyCurrentIntentOrDispositionCodeCode,
        "description" .= bankruptcyCurrentIntentOrDispositionCodeDescription
      ]

-- | Construct a value of type 'BankruptcyCurrentIntentOrDispositionCode' (by applying it's required fields, if any)
mkBankruptcyCurrentIntentOrDispositionCode ::
  BankruptcyCurrentIntentOrDispositionCode
mkBankruptcyCurrentIntentOrDispositionCode =
  BankruptcyCurrentIntentOrDispositionCode
    { bankruptcyCurrentIntentOrDispositionCodeCode = Nothing,
      bankruptcyCurrentIntentOrDispositionCodeDescription = Nothing
    }

-- ** BankruptcyPriorIntentOrDispositionCode

-- | BankruptcyPriorIntentOrDispositionCode
-- Chapter type and status of the bankruptcy just prior to that in the Current Intent/Disposition Code field
data BankruptcyPriorIntentOrDispositionCode = BankruptcyPriorIntentOrDispositionCode
  { -- | "code" - Code value
    bankruptcyPriorIntentOrDispositionCodeCode :: !(Maybe Text),
    -- | "description"
    bankruptcyPriorIntentOrDispositionCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON BankruptcyPriorIntentOrDispositionCode
instance A.FromJSON BankruptcyPriorIntentOrDispositionCode where
  parseJSON = A.withObject "BankruptcyPriorIntentOrDispositionCode" $ \o ->
    BankruptcyPriorIntentOrDispositionCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON BankruptcyPriorIntentOrDispositionCode
instance A.ToJSON BankruptcyPriorIntentOrDispositionCode where
  toJSON BankruptcyPriorIntentOrDispositionCode {..} =
    _omitNulls
      [ "code" .= bankruptcyPriorIntentOrDispositionCodeCode,
        "description" .= bankruptcyPriorIntentOrDispositionCodeDescription
      ]

-- | Construct a value of type 'BankruptcyPriorIntentOrDispositionCode' (by applying it's required fields, if any)
mkBankruptcyPriorIntentOrDispositionCode ::
  BankruptcyPriorIntentOrDispositionCode
mkBankruptcyPriorIntentOrDispositionCode =
  BankruptcyPriorIntentOrDispositionCode
    { bankruptcyPriorIntentOrDispositionCodeCode = Nothing,
      bankruptcyPriorIntentOrDispositionCodeDescription = Nothing
    }

-- ** Collection

-- | Collection
-- Provides information about any collection item
data Collection = Collection
  { -- | "industryCode" - Industry code
    collectionIndustryCode :: !(Maybe Text),
    -- | "customerNumber" - Collection agency customer number
    collectionCustomerNumber :: !(Maybe Text),
    -- | "clientNameOrNumber" - Original creditor name/number
    collectionClientNameOrNumber :: !(Maybe Text),
    -- | "statusCode"
    collectionStatusCode :: !(Maybe CollectionStatusCode),
    -- | "narrativeCodes"
    collectionNarrativeCodes :: !(Maybe [A.Value]),
    -- | "rawNarrativeCodes" - Raw narrative codes included in the report
    collectionRawNarrativeCodes :: !(Maybe [Text]),
    -- | "indicator" - Automated Update Indicator
    collectionIndicator :: !(Maybe Text),
    -- | "dateReported" - Date the collection was reported to Equifax
    collectionDateReported :: !(Maybe Date),
    -- | "dateAssigned" - Date Assigned
    collectionDateAssigned :: !(Maybe Date),
    -- | "originalAmount" - Original amount in USD
    collectionOriginalAmount :: !(Maybe Double),
    -- | "statusDate" - Current Status Date
    collectionStatusDate :: !(Maybe Date),
    -- | "balance" - Balance in USD
    collectionBalance :: !(Maybe Double),
    -- | "lastPaymentDate" - Date of most recent payment
    collectionLastPaymentDate :: !(Maybe Date),
    -- | "dateOfFirstDelinquency" - Indicates the date of first delinquency as reported by the original creditor
    collectionDateOfFirstDelinquency :: !(Maybe Date),
    -- | "accountNumber" - Account number
    collectionAccountNumber :: !(Maybe Text),
    -- | "accountDesignatorCode"
    collectionAccountDesignatorCode :: !(Maybe AccountDesignatorCode),
    -- | "creditorClassificationCode"
    collectionCreditorClassificationCode :: !(Maybe CreditorClassificationCode)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Collection
instance A.FromJSON Collection where
  parseJSON = A.withObject "Collection" $ \o ->
    Collection
      <$> (o .:? "industryCode")
      <*> (o .:? "customerNumber")
      <*> (o .:? "clientNameOrNumber")
      <*> (o .:? "statusCode")
      <*> (o .:? "narrativeCodes")
      <*> (o .:? "rawNarrativeCodes")
      <*> (o .:? "indicator")
      <*> (o .:? "dateReported")
      <*> (o .:? "dateAssigned")
      <*> (o .:? "originalAmount")
      <*> (o .:? "statusDate")
      <*> (o .:? "balance")
      <*> (o .:? "lastPaymentDate")
      <*> (o .:? "dateOfFirstDelinquency")
      <*> (o .:? "accountNumber")
      <*> (o .:? "accountDesignatorCode")
      <*> (o .:? "creditorClassificationCode")

-- | ToJSON Collection
instance A.ToJSON Collection where
  toJSON Collection {..} =
    _omitNulls
      [ "industryCode" .= collectionIndustryCode,
        "customerNumber" .= collectionCustomerNumber,
        "clientNameOrNumber" .= collectionClientNameOrNumber,
        "statusCode" .= collectionStatusCode,
        "narrativeCodes" .= collectionNarrativeCodes,
        "rawNarrativeCodes" .= collectionRawNarrativeCodes,
        "indicator" .= collectionIndicator,
        "dateReported" .= collectionDateReported,
        "dateAssigned" .= collectionDateAssigned,
        "originalAmount" .= collectionOriginalAmount,
        "statusDate" .= collectionStatusDate,
        "balance" .= collectionBalance,
        "lastPaymentDate" .= collectionLastPaymentDate,
        "dateOfFirstDelinquency" .= collectionDateOfFirstDelinquency,
        "accountNumber" .= collectionAccountNumber,
        "accountDesignatorCode" .= collectionAccountDesignatorCode,
        "creditorClassificationCode" .= collectionCreditorClassificationCode
      ]

-- | Construct a value of type 'Collection' (by applying it's required fields, if any)
mkCollection ::
  Collection
mkCollection =
  Collection
    { collectionIndustryCode = Nothing,
      collectionCustomerNumber = Nothing,
      collectionClientNameOrNumber = Nothing,
      collectionStatusCode = Nothing,
      collectionNarrativeCodes = Nothing,
      collectionRawNarrativeCodes = Nothing,
      collectionIndicator = Nothing,
      collectionDateReported = Nothing,
      collectionDateAssigned = Nothing,
      collectionOriginalAmount = Nothing,
      collectionStatusDate = Nothing,
      collectionBalance = Nothing,
      collectionLastPaymentDate = Nothing,
      collectionDateOfFirstDelinquency = Nothing,
      collectionAccountNumber = Nothing,
      collectionAccountDesignatorCode = Nothing,
      collectionCreditorClassificationCode = Nothing
    }

-- ** CollectionStatusCode

-- | CollectionStatusCode
-- Current status of the collection
data CollectionStatusCode = CollectionStatusCode
  { -- | "code" - Code value
    collectionStatusCodeCode :: !(Maybe Text),
    -- | "description" - - D: UNPAID Subject has not satisfied debt. - F: FINANCIAL COUNSELOR Identifies that a subject is receiving professional guidance on financial matters, and is under a payment plan. - J: ADJUSTMENT Settlement of a debt in which full payment is not made or when the amount involved is not certain. - M: WAGE EARNER Chapter 13 (Debtors with regular income). This chapter allows an individual to reorganize finances and protect assets while a court approved repayment plan is in effect. - N: NEW LISTING Collection account which has just been turned over for collection of past due debt. - P: PAID Subject has satisfied debt. - S: ACCOUNT DISPUTED Merchant and consumer disagree on various particulars regarding merchandise, terms of agreement or amount owing. - T: PAYMENT Subject submits portion of money owing. - U: STATUS UNKNOWN Indicates status is not verified. - X: CHECKED As of the date reported, the balance was not paid and the account was verified at the request of the consumer via a dispute. - Z: IN BANKRUPTCY The legal process under the Federal Bankruptcy Act by which debtors are granted some form of relief from their financial obligations.
    collectionStatusCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON CollectionStatusCode
instance A.FromJSON CollectionStatusCode where
  parseJSON = A.withObject "CollectionStatusCode" $ \o ->
    CollectionStatusCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON CollectionStatusCode
instance A.ToJSON CollectionStatusCode where
  toJSON CollectionStatusCode {..} =
    _omitNulls
      [ "code" .= collectionStatusCodeCode,
        "description" .= collectionStatusCodeDescription
      ]

-- | Construct a value of type 'CollectionStatusCode' (by applying it's required fields, if any)
mkCollectionStatusCode ::
  CollectionStatusCode
mkCollectionStatusCode =
  CollectionStatusCode
    { collectionStatusCodeCode = Nothing,
      collectionStatusCodeDescription = Nothing
    }

-- ** ConsumerCreditReportAlertContacts

-- | ConsumerCreditReportAlertContacts
data ConsumerCreditReportAlertContacts = ConsumerCreditReportAlertContacts
  { -- | "alertType"
    consumerCreditReportAlertContactsAlertType :: !(Maybe ConsumerCreditReportAlertType),
    -- | "dateReported" - Date of reported alert
    consumerCreditReportAlertContactsDateReported :: !(Maybe Date),
    -- | "effectiveDate" - Date the contact info became effective
    consumerCreditReportAlertContactsEffectiveDate :: !(Maybe Date),
    -- | "status" -  - N: No Contact Information Provided
    consumerCreditReportAlertContactsStatus :: !(Maybe Text),
    -- | "telephoneNumbers"
    consumerCreditReportAlertContactsTelephoneNumbers :: !(Maybe [ConsumerCreditReportTelephoneNumbers]),
    -- | "address"
    consumerCreditReportAlertContactsAddress :: !(Maybe AlertContactAddress),
    -- | "additionalInformation" - Free text with extra info regarding the contact
    consumerCreditReportAlertContactsAdditionalInformation :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlertContacts
instance A.FromJSON ConsumerCreditReportAlertContacts where
  parseJSON = A.withObject "ConsumerCreditReportAlertContacts" $ \o ->
    ConsumerCreditReportAlertContacts
      <$> (o .:? "alertType")
      <*> (o .:? "dateReported")
      <*> (o .:? "effectiveDate")
      <*> (o .:? "status")
      <*> (o .:? "telephoneNumbers")
      <*> (o .:? "address")
      <*> (o .:? "additionalInformation")

-- | ToJSON ConsumerCreditReportAlertContacts
instance A.ToJSON ConsumerCreditReportAlertContacts where
  toJSON ConsumerCreditReportAlertContacts {..} =
    _omitNulls
      [ "alertType" .= consumerCreditReportAlertContactsAlertType,
        "dateReported" .= consumerCreditReportAlertContactsDateReported,
        "effectiveDate" .= consumerCreditReportAlertContactsEffectiveDate,
        "status" .= consumerCreditReportAlertContactsStatus,
        "telephoneNumbers" .= consumerCreditReportAlertContactsTelephoneNumbers,
        "address" .= consumerCreditReportAlertContactsAddress,
        "additionalInformation" .= consumerCreditReportAlertContactsAdditionalInformation
      ]

-- | Construct a value of type 'ConsumerCreditReportAlertContacts' (by applying it's required fields, if any)
mkConsumerCreditReportAlertContacts ::
  ConsumerCreditReportAlertContacts
mkConsumerCreditReportAlertContacts =
  ConsumerCreditReportAlertContacts
    { consumerCreditReportAlertContactsAlertType = Nothing,
      consumerCreditReportAlertContactsDateReported = Nothing,
      consumerCreditReportAlertContactsEffectiveDate = Nothing,
      consumerCreditReportAlertContactsStatus = Nothing,
      consumerCreditReportAlertContactsTelephoneNumbers = Nothing,
      consumerCreditReportAlertContactsAddress = Nothing,
      consumerCreditReportAlertContactsAdditionalInformation = Nothing
    }

-- ** ConsumerCreditReportAlertType

-- | ConsumerCreditReportAlertType
-- Types of reported alerts
data ConsumerCreditReportAlertType = ConsumerCreditReportAlertType
  { -- | "code" - Code value
    consumerCreditReportAlertTypeCode :: !(Maybe Text),
    -- | "description" -  - E: Extended Fraud - L: Initial Fraud - M: Military
    consumerCreditReportAlertTypeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlertType
instance A.FromJSON ConsumerCreditReportAlertType where
  parseJSON = A.withObject "ConsumerCreditReportAlertType" $ \o ->
    ConsumerCreditReportAlertType
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportAlertType
instance A.ToJSON ConsumerCreditReportAlertType where
  toJSON ConsumerCreditReportAlertType {..} =
    _omitNulls
      [ "code" .= consumerCreditReportAlertTypeCode,
        "description" .= consumerCreditReportAlertTypeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportAlertType' (by applying it's required fields, if any)
mkConsumerCreditReportAlertType ::
  ConsumerCreditReportAlertType
mkConsumerCreditReportAlertType =
  ConsumerCreditReportAlertType
    { consumerCreditReportAlertTypeCode = Nothing,
      consumerCreditReportAlertTypeDescription = Nothing
    }

-- ** ConsumerCreditReportTelephoneNumbers

-- | ConsumerCreditReportTelephoneNumbers
-- Contact's telephone information
data ConsumerCreditReportTelephoneNumbers = ConsumerCreditReportTelephoneNumbers
  { -- | "telephoneNumberType"
    consumerCreditReportTelephoneNumbersTelephoneNumberType :: !(Maybe ConsumerCreditReportTelephoneNumberType),
    -- | "countryCode" - the telephone&#39;s country code
    consumerCreditReportTelephoneNumbersCountryCode :: !(Maybe Text),
    -- | "telephoneNumber" - Telephone number including a valid area code
    consumerCreditReportTelephoneNumbersTelephoneNumber :: !(Maybe Text),
    -- | "extension" - Telephone extension if it&#39;s present
    consumerCreditReportTelephoneNumbersExtension :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportTelephoneNumbers
instance A.FromJSON ConsumerCreditReportTelephoneNumbers where
  parseJSON = A.withObject "ConsumerCreditReportTelephoneNumbers" $ \o ->
    ConsumerCreditReportTelephoneNumbers
      <$> (o .:? "telephoneNumberType")
      <*> (o .:? "countryCode")
      <*> (o .:? "telephoneNumber")
      <*> (o .:? "extension")

-- | ToJSON ConsumerCreditReportTelephoneNumbers
instance A.ToJSON ConsumerCreditReportTelephoneNumbers where
  toJSON ConsumerCreditReportTelephoneNumbers {..} =
    _omitNulls
      [ "telephoneNumberType" .= consumerCreditReportTelephoneNumbersTelephoneNumberType,
        "countryCode" .= consumerCreditReportTelephoneNumbersCountryCode,
        "telephoneNumber" .= consumerCreditReportTelephoneNumbersTelephoneNumber,
        "extension" .= consumerCreditReportTelephoneNumbersExtension
      ]

-- | Construct a value of type 'ConsumerCreditReportTelephoneNumbers' (by applying it's required fields, if any)
mkConsumerCreditReportTelephoneNumbers ::
  ConsumerCreditReportTelephoneNumbers
mkConsumerCreditReportTelephoneNumbers =
  ConsumerCreditReportTelephoneNumbers
    { consumerCreditReportTelephoneNumbersTelephoneNumberType = Nothing,
      consumerCreditReportTelephoneNumbersCountryCode = Nothing,
      consumerCreditReportTelephoneNumbersTelephoneNumber = Nothing,
      consumerCreditReportTelephoneNumbersExtension = Nothing
    }

--

-- ** AlertContactAddress

-- | AlertContactAddress
-- Contact's address information
data AlertContactAddress = AlertContactAddress
  { -- | "addressLine1" - House number, street name &amp; street type
    alertContactAddressAddressLine1 :: !(Maybe Text),
    -- | "addressLine2" - Apartment, condo, suite, or unit number
    alertContactAddressAddressLine2 :: !(Maybe Text),
    -- | "cityName" - City name
    alertContactAddressCityName :: !(Maybe Text),
    -- | "stateAbbreviation" - State name&#39;s abbreviation
    alertContactAddressStateAbbreviation :: !(Maybe Text),
    -- | "zipCode" - Zip code
    alertContactAddressZipCode :: !(Maybe Text),
    -- | "countryCode" - Country code
    alertContactAddressCountryCode :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AlertContactAddress
instance A.FromJSON AlertContactAddress where
  parseJSON = A.withObject "AlertContactAddress" $ \o ->
    AlertContactAddress
      <$> (o .:? "addressLine1")
      <*> (o .:? "addressLine2")
      <*> (o .:? "cityName")
      <*> (o .:? "stateAbbreviation")
      <*> (o .:? "zipCode")
      <*> (o .:? "countryCode")

-- | ToJSON AlertContactAddress
instance A.ToJSON AlertContactAddress where
  toJSON AlertContactAddress {..} =
    _omitNulls
      [ "addressLine1" .= alertContactAddressAddressLine1,
        "addressLine2" .= alertContactAddressAddressLine2,
        "cityName" .= alertContactAddressCityName,
        "stateAbbreviation" .= alertContactAddressStateAbbreviation,
        "zipCode" .= alertContactAddressZipCode,
        "countryCode" .= alertContactAddressCountryCode
      ]

-- | Construct a value of type 'AlertContactAddress' (by applying it's required fields, if any)
mkAlertContactAddress ::
  AlertContactAddress
mkAlertContactAddress =
  AlertContactAddress
    { alertContactAddressAddressLine1 = Nothing,
      alertContactAddressAddressLine2 = Nothing,
      alertContactAddressCityName = Nothing,
      alertContactAddressStateAbbreviation = Nothing,
      alertContactAddressZipCode = Nothing,
      alertContactAddressCountryCode = Nothing
    }

-- ** ConsumerCreditReportTelephoneNumberType

-- | ConsumerCreditReportTelephoneNumberType
-- Types of telephone numbers
data ConsumerCreditReportTelephoneNumberType = ConsumerCreditReportTelephoneNumberType
  { -- | "code" - Code value
    consumerCreditReportTelephoneNumberTypeCode :: !(Maybe Text),
    -- | "description" -  - C: Cellular - D: Daytime - E: Evening - P: Pager
    consumerCreditReportTelephoneNumberTypeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportTelephoneNumberType
instance A.FromJSON ConsumerCreditReportTelephoneNumberType where
  parseJSON = A.withObject "ConsumerCreditReportTelephoneNumberType" $ \o ->
    ConsumerCreditReportTelephoneNumberType
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportTelephoneNumberType
instance A.ToJSON ConsumerCreditReportTelephoneNumberType where
  toJSON ConsumerCreditReportTelephoneNumberType {..} =
    _omitNulls
      [ "code" .= consumerCreditReportTelephoneNumberTypeCode,
        "description" .= consumerCreditReportTelephoneNumberTypeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportTelephoneNumberType' (by applying it's required fields, if any)
mkConsumerCreditReportTelephoneNumberType ::
  ConsumerCreditReportTelephoneNumberType
mkConsumerCreditReportTelephoneNumberType =
  ConsumerCreditReportTelephoneNumberType
    { consumerCreditReportTelephoneNumberTypeCode = Nothing,
      consumerCreditReportTelephoneNumberTypeDescription = Nothing
    }

-- ** ConsumerCreditReportInquiries

-- | ConsumerCreditReportInquiries
data ConsumerCreditReportInquiries = ConsumerCreditReportInquiries
  { -- | "type" - Inquiries from customers requesting Equifax credit reports and other products
    consumerCreditReportInquiriesType :: !(Maybe Text),
    -- | "industryCode" - Customer&#39;s industry code
    consumerCreditReportInquiriesIndustryCode :: !(Maybe Text),
    -- | "inquiryDate" - Date of inquiry
    consumerCreditReportInquiriesInquiryDate :: !(Maybe Date),
    -- | "customerNumber" - Customer&#39;s member number who made the inquiry
    consumerCreditReportInquiriesCustomerNumber :: !(Maybe Text),
    -- | "customerName" - Customer&#39;s name who made the inquiry
    consumerCreditReportInquiriesCustomerName :: !(Maybe Text),
    -- | "expandedAccountTypeOrInquiryIntent"
    consumerCreditReportInquiriesExpandedAccountTypeOrInquiryIntent :: !(Maybe AccountTypeCode)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportInquiries
instance A.FromJSON ConsumerCreditReportInquiries where
  parseJSON = A.withObject "ConsumerCreditReportInquiries" $ \o ->
    ConsumerCreditReportInquiries
      <$> (o .:? "type")
      <*> (o .:? "industryCode")
      <*> (o .:? "inquiryDate")
      <*> (o .:? "customerNumber")
      <*> (o .:? "customerName")
      <*> (o .:? "expandedAccountTypeOrInquiryIntent")

-- | ToJSON ConsumerCreditReportInquiries
instance A.ToJSON ConsumerCreditReportInquiries where
  toJSON ConsumerCreditReportInquiries {..} =
    _omitNulls
      [ "type" .= consumerCreditReportInquiriesType,
        "industryCode" .= consumerCreditReportInquiriesIndustryCode,
        "inquiryDate" .= consumerCreditReportInquiriesInquiryDate,
        "customerNumber" .= consumerCreditReportInquiriesCustomerNumber,
        "customerName" .= consumerCreditReportInquiriesCustomerName,
        "expandedAccountTypeOrInquiryIntent" .= consumerCreditReportInquiriesExpandedAccountTypeOrInquiryIntent
      ]

-- | Construct a value of type 'ConsumerCreditReportInquiries' (by applying it's required fields, if any)
mkConsumerCreditReportInquiries ::
  ConsumerCreditReportInquiries
mkConsumerCreditReportInquiries =
  ConsumerCreditReportInquiries
    { consumerCreditReportInquiriesType = Nothing,
      consumerCreditReportInquiriesIndustryCode = Nothing,
      consumerCreditReportInquiriesInquiryDate = Nothing,
      consumerCreditReportInquiriesCustomerNumber = Nothing,
      consumerCreditReportInquiriesCustomerName = Nothing,
      consumerCreditReportInquiriesExpandedAccountTypeOrInquiryIntent = Nothing
    }

-- ** ConsumerCreditReportConsumerStatements

-- | ConsumerCreditReportConsumerStatements
data ConsumerCreditReportConsumerStatements = ConsumerCreditReportConsumerStatements
  { -- | "dateReported" - Date in format MMYYYY where MM is the Month and YYYY is the year
    consumerCreditReportConsumerStatementsDateReported :: !(Maybe Text),
    -- | "datePurged" - Date in format MMYYYY where MM is the Month and YYYY is the year
    consumerCreditReportConsumerStatementsDatePurged :: !(Maybe Text),
    -- | "statement" - Consumer Statement text
    consumerCreditReportConsumerStatementsStatement :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportConsumerStatements
instance A.FromJSON ConsumerCreditReportConsumerStatements where
  parseJSON = A.withObject "ConsumerCreditReportConsumerStatements" $ \o ->
    ConsumerCreditReportConsumerStatements
      <$> (o .:? "dateReported")
      <*> (o .:? "datePurged")
      <*> (o .:? "statement")

-- | ToJSON ConsumerCreditReportConsumerStatements
instance A.ToJSON ConsumerCreditReportConsumerStatements where
  toJSON ConsumerCreditReportConsumerStatements {..} =
    _omitNulls
      [ "dateReported" .= consumerCreditReportConsumerStatementsDateReported,
        "datePurged" .= consumerCreditReportConsumerStatementsDatePurged,
        "statement" .= consumerCreditReportConsumerStatementsStatement
      ]

-- | Construct a value of type 'ConsumerCreditReportConsumerStatements' (by applying it's required fields, if any)
mkConsumerCreditReportConsumerStatements ::
  ConsumerCreditReportConsumerStatements
mkConsumerCreditReportConsumerStatements =
  ConsumerCreditReportConsumerStatements
    { consumerCreditReportConsumerStatementsDateReported = Nothing,
      consumerCreditReportConsumerStatementsDatePurged = Nothing,
      consumerCreditReportConsumerStatementsStatement = Nothing
    }

-- ** E'Type

-- | Enum of 'Text' .
-- Determine what type of model is
data E'Type
  = -- | @"EDAS"@
    E'Type'EDAS
  | -- | @"FICO"@
    E'Type'FICO
  | -- | @"MARKETMAX"@
    E'Type'MARKETMAX
  | -- | @"IDSCORE"@
    E'Type'IDSCORE
  | -- | @"MODEL"@
    E'Type'MODEL
  | -- | @"RBP"@
    E'Type'RBP
  | -- | @"DODDFRANK"@
    E'Type'DODDFRANK
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON E'Type where toJSON = A.toJSON . fromE'Type

instance A.FromJSON E'Type where parseJSON o = P.either P.fail pure . toE'Type =<< A.parseJSON o

instance WH.ToHttpApiData E'Type where toQueryParam = WH.toQueryParam . fromE'Type

instance WH.FromHttpApiData E'Type where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toE'Type

instance MimeRender MimeMultipartFormData E'Type where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'E'Type' enum
fromE'Type :: E'Type -> Text
fromE'Type = \case
  E'Type'EDAS -> "EDAS"
  E'Type'FICO -> "FICO"
  E'Type'MARKETMAX -> "MARKETMAX"
  E'Type'IDSCORE -> "IDSCORE"
  E'Type'MODEL -> "MODEL"
  E'Type'RBP -> "RBP"
  E'Type'DODDFRANK -> "DODDFRANK"

-- | parse 'E'Type' enum
toE'Type :: Text -> P.Either String E'Type
toE'Type = \case
  "EDAS" -> P.Right E'Type'EDAS
  "FICO" -> P.Right E'Type'FICO
  "MARKETMAX" -> P.Right E'Type'MARKETMAX
  "IDSCORE" -> P.Right E'Type'IDSCORE
  "MODEL" -> P.Right E'Type'MODEL
  "RBP" -> P.Right E'Type'RBP
  "DODDFRANK" -> P.Right E'Type'DODDFRANK
  s -> P.Left $ "toE'Type: enum parse failure: " P.++ P.show s

-- ** ConsumerCreditReportModels

-- | ConsumerCreditReportModels
data ConsumerCreditReportModels = ConsumerCreditReportModels
  { -- | "type" - Determine what type of model is
    consumerCreditReportModelsType :: !(Maybe E'Type),
    -- | "modelNumber" - Model number code
    consumerCreditReportModelsModelNumber :: !(Maybe Text),
    -- | "FICOScoreIndicatorCode"
    consumerCreditReportModelsFicoScoreIndicatorCode :: !(Maybe ConsumerCreditReportFICOScoreIndicatorCode),
    -- | "score" - Numeric score returned by the model requested by the customer
    consumerCreditReportModelsScore :: !(Maybe Int),
    -- | "reasons" - Reason codes for the score that was returned
    consumerCreditReportModelsReasons :: !(Maybe [ConsumerCreditReportReasons]),
    -- | "inquiryKeyFactor"
    consumerCreditReportModelsInquiryKeyFactor :: !(Maybe ConsumerCreditReportInquiryKeyFactor),
    -- | "riskBasedPricingOrModel"
    consumerCreditReportModelsRiskBasedPricingOrModel :: !(Maybe ConsumerCreditReportRiskBasedPricingOrModel),
    -- | "rejects" - Model unable to score file; requested reject code return instead
    consumerCreditReportModelsRejects :: !(Maybe [ConsumerCreditReportRejects]),
    -- | "EDASRegionalIndicatorCode"
    consumerCreditReportModelsEdasRegionalIndicatorCode :: !(Maybe ConsumerCreditReportEDASRegionalIndicatorCode),
    -- | "EDASIndicatorCode"
    consumerCreditReportModelsEdasIndicatorCode :: !(Maybe ConsumerCreditReportEDASIndicatorCode),
    -- | "modelIDOrScorecard" - Scorecard that was used to produce the score returned
    consumerCreditReportModelsModelIdorScorecard :: !(Maybe Text),
    -- | "scoreNumberOrMarketMaxIndustryCode"
    consumerCreditReportModelsScoreNumberOrMarketMaxIndustryCode :: !(Maybe ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode),
    -- | "numericScoreIndicator" - Indicates whether the score is positive or negative
    consumerCreditReportModelsNumericScoreIndicator :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportModels
instance A.FromJSON ConsumerCreditReportModels where
  parseJSON = A.withObject "ConsumerCreditReportModels" $ \o ->
    ConsumerCreditReportModels
      <$> (o .:? "type")
      <*> (o .:? "modelNumber")
      <*> (o .:? "FICOScoreIndicatorCode")
      <*> (o .:? "score")
      <*> (o .:? "reasons")
      <*> (o .:? "inquiryKeyFactor")
      <*> (o .:? "riskBasedPricingOrModel")
      <*> (o .:? "rejects")
      <*> (o .:? "EDASRegionalIndicatorCode")
      <*> (o .:? "EDASIndicatorCode")
      <*> (o .:? "modelIDOrScorecard")
      <*> (o .:? "scoreNumberOrMarketMaxIndustryCode")
      <*> (o .:? "numericScoreIndicator")

-- | ToJSON ConsumerCreditReportModels
instance A.ToJSON ConsumerCreditReportModels where
  toJSON ConsumerCreditReportModels {..} =
    _omitNulls
      [ "type" .= consumerCreditReportModelsType,
        "modelNumber" .= consumerCreditReportModelsModelNumber,
        "FICOScoreIndicatorCode" .= consumerCreditReportModelsFicoScoreIndicatorCode,
        "score" .= consumerCreditReportModelsScore,
        "reasons" .= consumerCreditReportModelsReasons,
        "inquiryKeyFactor" .= consumerCreditReportModelsInquiryKeyFactor,
        "riskBasedPricingOrModel" .= consumerCreditReportModelsRiskBasedPricingOrModel,
        "rejects" .= consumerCreditReportModelsRejects,
        "EDASRegionalIndicatorCode" .= consumerCreditReportModelsEdasRegionalIndicatorCode,
        "EDASIndicatorCode" .= consumerCreditReportModelsEdasIndicatorCode,
        "modelIDOrScorecard" .= consumerCreditReportModelsModelIdorScorecard,
        "scoreNumberOrMarketMaxIndustryCode" .= consumerCreditReportModelsScoreNumberOrMarketMaxIndustryCode,
        "numericScoreIndicator" .= consumerCreditReportModelsNumericScoreIndicator
      ]

-- | Construct a value of type 'ConsumerCreditReportModels' (by applying it's required fields, if any)
mkConsumerCreditReportModels ::
  ConsumerCreditReportModels
mkConsumerCreditReportModels =
  ConsumerCreditReportModels
    { consumerCreditReportModelsType = Nothing,
      consumerCreditReportModelsModelNumber = Nothing,
      consumerCreditReportModelsFicoScoreIndicatorCode = Nothing,
      consumerCreditReportModelsScore = Nothing,
      consumerCreditReportModelsReasons = Nothing,
      consumerCreditReportModelsInquiryKeyFactor = Nothing,
      consumerCreditReportModelsRiskBasedPricingOrModel = Nothing,
      consumerCreditReportModelsRejects = Nothing,
      consumerCreditReportModelsEdasRegionalIndicatorCode = Nothing,
      consumerCreditReportModelsEdasIndicatorCode = Nothing,
      consumerCreditReportModelsModelIdorScorecard = Nothing,
      consumerCreditReportModelsScoreNumberOrMarketMaxIndustryCode = Nothing,
      consumerCreditReportModelsNumericScoreIndicator = Nothing
    }

-- ** ConsumerCreditReportOnlineDirectory

-- | ConsumerCreditReportOnlineDirectory
data ConsumerCreditReportOnlineDirectory = ConsumerCreditReportOnlineDirectory
  { -- | "customerNumber" - Equifax customer number
    consumerCreditReportOnlineDirectoryCustomerNumber :: !(Maybe Text),
    -- | "customerName" - Customer&#39;s name
    consumerCreditReportOnlineDirectoryCustomerName :: !(Maybe Text),
    -- | "telephoneNumber" - Telephone number including a valid area code
    consumerCreditReportOnlineDirectoryTelephoneNumber :: !(Maybe Text),
    -- | "addressLine1" - Customer address line 1
    consumerCreditReportOnlineDirectoryAddressLine1 :: !(Maybe Text),
    -- | "addressLine2" - Customer address line 2
    consumerCreditReportOnlineDirectoryAddressLine2 :: !(Maybe Text),
    -- | "city" - The city name
    consumerCreditReportOnlineDirectoryCity :: !(Maybe Text),
    -- | "stateAbbreviation" - State name&#39;s abbreviation
    consumerCreditReportOnlineDirectoryStateAbbreviation :: !(Maybe Text),
    -- | "zipCode" - Zip code
    consumerCreditReportOnlineDirectoryZipCode :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportOnlineDirectory
instance A.FromJSON ConsumerCreditReportOnlineDirectory where
  parseJSON = A.withObject "ConsumerCreditReportOnlineDirectory" $ \o ->
    ConsumerCreditReportOnlineDirectory
      <$> (o .:? "customerNumber")
      <*> (o .:? "customerName")
      <*> (o .:? "telephoneNumber")
      <*> (o .:? "addressLine1")
      <*> (o .:? "addressLine2")
      <*> (o .:? "city")
      <*> (o .:? "stateAbbreviation")
      <*> (o .:? "zipCode")

-- | ToJSON ConsumerCreditReportOnlineDirectory
instance A.ToJSON ConsumerCreditReportOnlineDirectory where
  toJSON ConsumerCreditReportOnlineDirectory {..} =
    _omitNulls
      [ "customerNumber" .= consumerCreditReportOnlineDirectoryCustomerNumber,
        "customerName" .= consumerCreditReportOnlineDirectoryCustomerName,
        "telephoneNumber" .= consumerCreditReportOnlineDirectoryTelephoneNumber,
        "addressLine1" .= consumerCreditReportOnlineDirectoryAddressLine1,
        "addressLine2" .= consumerCreditReportOnlineDirectoryAddressLine2,
        "city" .= consumerCreditReportOnlineDirectoryCity,
        "stateAbbreviation" .= consumerCreditReportOnlineDirectoryStateAbbreviation,
        "zipCode" .= consumerCreditReportOnlineDirectoryZipCode
      ]

-- | Construct a value of type 'ConsumerCreditReportOnlineDirectory' (by applying it's required fields, if any)
mkConsumerCreditReportOnlineDirectory ::
  ConsumerCreditReportOnlineDirectory
mkConsumerCreditReportOnlineDirectory =
  ConsumerCreditReportOnlineDirectory
    { consumerCreditReportOnlineDirectoryCustomerNumber = Nothing,
      consumerCreditReportOnlineDirectoryCustomerName = Nothing,
      consumerCreditReportOnlineDirectoryTelephoneNumber = Nothing,
      consumerCreditReportOnlineDirectoryAddressLine1 = Nothing,
      consumerCreditReportOnlineDirectoryAddressLine2 = Nothing,
      consumerCreditReportOnlineDirectoryCity = Nothing,
      consumerCreditReportOnlineDirectoryStateAbbreviation = Nothing,
      consumerCreditReportOnlineDirectoryZipCode = Nothing
    }

-- ** ConsumerCreditReportHitCode

-- | ConsumerCreditReportHitCode
-- ** ConsumerCreditReportIdentification
-- | ConsumerCreditReportIdentification
-- Information as to the subject's consumer report SSN and subject's SSN submitted in the inquiry
data ConsumerCreditReportIdentification = ConsumerCreditReportIdentification
  { -- | "subjectAge" - Subject&#39;s age
    consumerCreditReportIdentificationSubjectAge :: !(Maybe Text),
    -- | "subjectSocialNum" - Last four digits of the Social Security Number may be masked with zeros for specific customers or industry codes to secure the consumers SSN
    consumerCreditReportIdentificationSubjectSocialNum :: !(Maybe Text),
    -- | "socialNumConfirmed" - \&quot;Confirmation\&quot; of the Social Security Number is defined as having been received by three (3) major data suppliers. The SSN Confirmed indicator does not mean or imply the Social Security number has been validated through the Social Security Administration&#39;s records.      - Y: Yes      - N: No      - Blank: No inquiry SSN/Not available
    consumerCreditReportIdentificationSocialNumConfirmed :: !(Maybe Text),
    -- | "socialMatchFlags" - Social Match Flags* (one per SSN digit)    - Y: Byte (digit) match    - N: Byte (digit) Not a match    - Blank: No inquiry SSN/Not available    The field will not be returned on reports when the customer number is activated for \&quot;Social Security Number Protect\&quot; and the social security number in the inquiry does not match what is on the consumer report.  Social Match Flags is an optional service offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation of the service.
    consumerCreditReportIdentificationSocialMatchFlags :: !(Maybe Text),
    -- | "inquirySocialNum" - Inquiry social security number
    consumerCreditReportIdentificationInquirySocialNum :: !(Maybe Text),
    -- | "inquirySocialNumStateIssued" - State code where the SSN was issued.  It may contain a \&quot;RR\&quot;, indicating that the SSN was issued to a railroad employe. Discontinued July 1, 1963.
    consumerCreditReportIdentificationInquirySocialNumStateIssued :: !(Maybe Text),
    -- | "inquirySocialNumYearIssued" - Inquiry SSN date issued   The actual year or \&quot;Prior to\&quot;.   P 51 means Prior to 1951
    consumerCreditReportIdentificationInquirySocialNumYearIssued :: !(Maybe Text),
    -- | "inquirySocialNumYearOfDeath" - Inquiry SSN death date.   The actual year or \&quot;Prior to\&quot;.   P 91 means Prior to 1991
    consumerCreditReportIdentificationInquirySocialNumYearOfDeath :: !(Maybe Text),
    -- | "inquirySocialNumStateOfDeath" - If party is deceased this is the state it was reported in
    consumerCreditReportIdentificationInquirySocialNumStateOfDeath :: !(Maybe Text),
    -- | "socialNumMatch" - Shows if the SSN match byte-to-byte   - Y: Match    - N: Not a match  The field will not be returned on reports when the customer number is activated for \&quot;Social Security Number Protect\&quot; and the social security number in the inquiry does not match what is on the consumer report. SSN Match is an optional service offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation of the service.
    consumerCreditReportIdentificationSocialNumMatch :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportIdentification
instance A.FromJSON ConsumerCreditReportIdentification where
  parseJSON = A.withObject "ConsumerCreditReportIdentification" $ \o ->
    ConsumerCreditReportIdentification
      <$> (o .:? "subjectAge")
      <*> (o .:? "subjectSocialNum")
      <*> (o .:? "socialNumConfirmed")
      <*> (o .:? "socialMatchFlags")
      <*> (o .:? "inquirySocialNum")
      <*> (o .:? "inquirySocialNumStateIssued")
      <*> (o .:? "inquirySocialNumYearIssued")
      <*> (o .:? "inquirySocialNumYearOfDeath")
      <*> (o .:? "inquirySocialNumStateOfDeath")
      <*> (o .:? "socialNumMatch")

-- | ToJSON ConsumerCreditReportIdentification
instance A.ToJSON ConsumerCreditReportIdentification where
  toJSON ConsumerCreditReportIdentification {..} =
    _omitNulls
      [ "subjectAge" .= consumerCreditReportIdentificationSubjectAge,
        "subjectSocialNum" .= consumerCreditReportIdentificationSubjectSocialNum,
        "socialNumConfirmed" .= consumerCreditReportIdentificationSocialNumConfirmed,
        "socialMatchFlags" .= consumerCreditReportIdentificationSocialMatchFlags,
        "inquirySocialNum" .= consumerCreditReportIdentificationInquirySocialNum,
        "inquirySocialNumStateIssued" .= consumerCreditReportIdentificationInquirySocialNumStateIssued,
        "inquirySocialNumYearIssued" .= consumerCreditReportIdentificationInquirySocialNumYearIssued,
        "inquirySocialNumYearOfDeath" .= consumerCreditReportIdentificationInquirySocialNumYearOfDeath,
        "inquirySocialNumStateOfDeath" .= consumerCreditReportIdentificationInquirySocialNumStateOfDeath,
        "socialNumMatch" .= consumerCreditReportIdentificationSocialNumMatch
      ]

-- | Construct a value of type 'ConsumerCreditReportIdentification' (by applying it's required fields, if any)
mkConsumerCreditReportIdentification ::
  ConsumerCreditReportIdentification
mkConsumerCreditReportIdentification =
  ConsumerCreditReportIdentification
    { consumerCreditReportIdentificationSubjectAge = Nothing,
      consumerCreditReportIdentificationSubjectSocialNum = Nothing,
      consumerCreditReportIdentificationSocialNumConfirmed = Nothing,
      consumerCreditReportIdentificationSocialMatchFlags = Nothing,
      consumerCreditReportIdentificationInquirySocialNum = Nothing,
      consumerCreditReportIdentificationInquirySocialNumStateIssued = Nothing,
      consumerCreditReportIdentificationInquirySocialNumYearIssued = Nothing,
      consumerCreditReportIdentificationInquirySocialNumYearOfDeath = Nothing,
      consumerCreditReportIdentificationInquirySocialNumStateOfDeath = Nothing,
      consumerCreditReportIdentificationSocialNumMatch = Nothing
    }

-- ** ConsumerCreditReportAttributes1

-- | ConsumerCreditReportAttributes1
-- Model Attribute data
data ConsumerCreditReportAttributes1 = ConsumerCreditReportAttributes1
  { -- | "modelNumber" - Model number
    consumerCreditReportAttributes1ModelNumber :: !(Maybe Text),
    -- | "numberOfVariableFields" - Indicates the number of fields with variables being returned
    consumerCreditReportAttributes1NumberOfVariableFields :: !(Maybe Text),
    -- | "attributes" - Fields with variables being returned
    consumerCreditReportAttributes1Attributes :: !(Maybe [ConsumerCreditReportAttributes])
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAttributes1
instance A.FromJSON ConsumerCreditReportAttributes1 where
  parseJSON = A.withObject "ConsumerCreditReportAttributes1" $ \o ->
    ConsumerCreditReportAttributes1
      <$> (o .:? "modelNumber")
      <*> (o .:? "numberOfVariableFields")
      <*> (o .:? "attributes")

-- | ToJSON ConsumerCreditReportAttributes1
instance A.ToJSON ConsumerCreditReportAttributes1 where
  toJSON ConsumerCreditReportAttributes1 {..} =
    _omitNulls
      [ "modelNumber" .= consumerCreditReportAttributes1ModelNumber,
        "numberOfVariableFields" .= consumerCreditReportAttributes1NumberOfVariableFields,
        "attributes" .= consumerCreditReportAttributes1Attributes
      ]

-- | Construct a value of type 'ConsumerCreditReportAttributes1' (by applying it's required fields, if any)
mkConsumerCreditReportAttributes1 ::
  ConsumerCreditReportAttributes1
mkConsumerCreditReportAttributes1 =
  ConsumerCreditReportAttributes1
    { consumerCreditReportAttributes1ModelNumber = Nothing,
      consumerCreditReportAttributes1NumberOfVariableFields = Nothing,
      consumerCreditReportAttributes1Attributes = Nothing
    }

-- ** ConsumerCreditReportOnlineGeoCode

-- | ConsumerCreditReportOnlineGeoCode
data ConsumerCreditReportOnlineGeoCode = ConsumerCreditReportOnlineGeoCode
  { -- | "geoSMSACode" - Metropolitan Statistical Area Code
    consumerCreditReportOnlineGeoCodeGeoSmsaCode :: !(Maybe Text),
    -- | "geoStateCode" - State code
    consumerCreditReportOnlineGeoCodeGeoStateCode :: !(Maybe Text),
    -- | "geoCountyCode" - County code
    consumerCreditReportOnlineGeoCodeGeoCountyCode :: !(Maybe Text),
    -- | "geoCensusTract" - Census tract code
    consumerCreditReportOnlineGeoCodeGeoCensusTract :: !(Maybe Text),
    -- | "geoSuffix" - Census tract code suffix
    consumerCreditReportOnlineGeoCodeGeoSuffix :: !(Maybe Text),
    -- | "geoBlockGroup" - Block group code.
    consumerCreditReportOnlineGeoCodeGeoBlockGroup :: !(Maybe Text),
    -- | "streetNumber" - Street number
    consumerCreditReportOnlineGeoCodeStreetNumber :: !(Maybe Text),
    -- | "streetName" - Street name
    consumerCreditReportOnlineGeoCodeStreetName :: !(Maybe Text),
    -- | "streetTypeOrDirection" - Street Type or Direction - Drive, Street, Road, etc. East, West, etc
    consumerCreditReportOnlineGeoCodeStreetTypeOrDirection :: !(Maybe Text),
    -- | "geoSMSA5DigitCode" - Standard Metropolitan Statistical Area Code
    consumerCreditReportOnlineGeoCodeGeoSmsa5DigitCode :: !(Maybe Text),
    -- | "city" - City name
    consumerCreditReportOnlineGeoCodeCity :: !(Maybe Text),
    -- | "stateAbbreviation" - State name&#39;s abbreviation
    consumerCreditReportOnlineGeoCodeStateAbbreviation :: !(Maybe Text),
    -- | "zipCode" - Zip code
    consumerCreditReportOnlineGeoCodeZipCode :: !(Maybe Text),
    -- | "typeOfAddress"
    consumerCreditReportOnlineGeoCodeTypeOfAddress :: !(Maybe ConsumerCreditReportTypeOfAddress),
    -- | "returnCode1"
    consumerCreditReportOnlineGeoCodeReturnCode1 :: !(Maybe ConsumerCreditReportReturnCode1),
    -- | "returnCode2"
    consumerCreditReportOnlineGeoCodeReturnCode2 :: !(Maybe ConsumerCreditReportReturnCode2),
    -- | "returnCode3"
    consumerCreditReportOnlineGeoCodeReturnCode3 :: !(Maybe ConsumerCreditReportReturnCode3),
    -- | "returnCode4"
    consumerCreditReportOnlineGeoCodeReturnCode4 :: !(Maybe ConsumerCreditReportReturnCode4),
    -- | "microVisionCode"
    consumerCreditReportOnlineGeoCodeMicroVisionCode :: !(Maybe Text),
    -- | "microVisionReturnCode"
    consumerCreditReportOnlineGeoCodeMicroVisionReturnCode :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportOnlineGeoCode
instance A.FromJSON ConsumerCreditReportOnlineGeoCode where
  parseJSON = A.withObject "ConsumerCreditReportOnlineGeoCode" $ \o ->
    ConsumerCreditReportOnlineGeoCode
      <$> (o .:? "geoSMSACode")
      <*> (o .:? "geoStateCode")
      <*> (o .:? "geoCountyCode")
      <*> (o .:? "geoCensusTract")
      <*> (o .:? "geoSuffix")
      <*> (o .:? "geoBlockGroup")
      <*> (o .:? "streetNumber")
      <*> (o .:? "streetName")
      <*> (o .:? "streetTypeOrDirection")
      <*> (o .:? "geoSMSA5DigitCode")
      <*> (o .:? "city")
      <*> (o .:? "stateAbbreviation")
      <*> (o .:? "zipCode")
      <*> (o .:? "typeOfAddress")
      <*> (o .:? "returnCode1")
      <*> (o .:? "returnCode2")
      <*> (o .:? "returnCode3")
      <*> (o .:? "returnCode4")
      <*> (o .:? "microVisionCode")
      <*> (o .:? "microVisionReturnCode")

-- | ToJSON ConsumerCreditReportOnlineGeoCode
instance A.ToJSON ConsumerCreditReportOnlineGeoCode where
  toJSON ConsumerCreditReportOnlineGeoCode {..} =
    _omitNulls
      [ "geoSMSACode" .= consumerCreditReportOnlineGeoCodeGeoSmsaCode,
        "geoStateCode" .= consumerCreditReportOnlineGeoCodeGeoStateCode,
        "geoCountyCode" .= consumerCreditReportOnlineGeoCodeGeoCountyCode,
        "geoCensusTract" .= consumerCreditReportOnlineGeoCodeGeoCensusTract,
        "geoSuffix" .= consumerCreditReportOnlineGeoCodeGeoSuffix,
        "geoBlockGroup" .= consumerCreditReportOnlineGeoCodeGeoBlockGroup,
        "streetNumber" .= consumerCreditReportOnlineGeoCodeStreetNumber,
        "streetName" .= consumerCreditReportOnlineGeoCodeStreetName,
        "streetTypeOrDirection" .= consumerCreditReportOnlineGeoCodeStreetTypeOrDirection,
        "geoSMSA5DigitCode" .= consumerCreditReportOnlineGeoCodeGeoSmsa5DigitCode,
        "city" .= consumerCreditReportOnlineGeoCodeCity,
        "stateAbbreviation" .= consumerCreditReportOnlineGeoCodeStateAbbreviation,
        "zipCode" .= consumerCreditReportOnlineGeoCodeZipCode,
        "typeOfAddress" .= consumerCreditReportOnlineGeoCodeTypeOfAddress,
        "returnCode1" .= consumerCreditReportOnlineGeoCodeReturnCode1,
        "returnCode2" .= consumerCreditReportOnlineGeoCodeReturnCode2,
        "returnCode3" .= consumerCreditReportOnlineGeoCodeReturnCode3,
        "returnCode4" .= consumerCreditReportOnlineGeoCodeReturnCode4,
        "microVisionCode" .= consumerCreditReportOnlineGeoCodeMicroVisionCode,
        "microVisionReturnCode" .= consumerCreditReportOnlineGeoCodeMicroVisionReturnCode
      ]

-- | Construct a value of type 'ConsumerCreditReportOnlineGeoCode' (by applying it's required fields, if any)
mkConsumerCreditReportOnlineGeoCode ::
  ConsumerCreditReportOnlineGeoCode
mkConsumerCreditReportOnlineGeoCode =
  ConsumerCreditReportOnlineGeoCode
    { consumerCreditReportOnlineGeoCodeGeoSmsaCode = Nothing,
      consumerCreditReportOnlineGeoCodeGeoStateCode = Nothing,
      consumerCreditReportOnlineGeoCodeGeoCountyCode = Nothing,
      consumerCreditReportOnlineGeoCodeGeoCensusTract = Nothing,
      consumerCreditReportOnlineGeoCodeGeoSuffix = Nothing,
      consumerCreditReportOnlineGeoCodeGeoBlockGroup = Nothing,
      consumerCreditReportOnlineGeoCodeStreetNumber = Nothing,
      consumerCreditReportOnlineGeoCodeStreetName = Nothing,
      consumerCreditReportOnlineGeoCodeStreetTypeOrDirection = Nothing,
      consumerCreditReportOnlineGeoCodeGeoSmsa5DigitCode = Nothing,
      consumerCreditReportOnlineGeoCodeCity = Nothing,
      consumerCreditReportOnlineGeoCodeStateAbbreviation = Nothing,
      consumerCreditReportOnlineGeoCodeZipCode = Nothing,
      consumerCreditReportOnlineGeoCodeTypeOfAddress = Nothing,
      consumerCreditReportOnlineGeoCodeReturnCode1 = Nothing,
      consumerCreditReportOnlineGeoCodeReturnCode2 = Nothing,
      consumerCreditReportOnlineGeoCodeReturnCode3 = Nothing,
      consumerCreditReportOnlineGeoCodeReturnCode4 = Nothing,
      consumerCreditReportOnlineGeoCodeMicroVisionCode = Nothing,
      consumerCreditReportOnlineGeoCodeMicroVisionReturnCode = Nothing
    }

-- ** ConsumerCreditReportConsumerReferralLocation

-- | ConsumerCreditReportConsumerReferralLocation
-- Provides contact information regarding the data contained in the report
data ConsumerCreditReportConsumerReferralLocation = ConsumerCreditReportConsumerReferralLocation
  { -- | "bureauCode" - Refereal Bureau Code
    consumerCreditReportConsumerReferralLocationBureauCode :: !(Maybe Text),
    -- | "bureauName" - Referral Bureau Name
    consumerCreditReportConsumerReferralLocationBureauName :: !(Maybe Text),
    -- | "address"
    consumerCreditReportConsumerReferralLocationAddress :: !(Maybe ConsumerCreditReportConsumerReferralLocationAddress),
    -- | "telephoneNumber"
    consumerCreditReportConsumerReferralLocationTelephoneNumber :: !(Maybe ConsumerCreditReportConsumerReferralLocationTelephoneNumber)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportConsumerReferralLocation
instance A.FromJSON ConsumerCreditReportConsumerReferralLocation where
  parseJSON = A.withObject "ConsumerCreditReportConsumerReferralLocation" $ \o ->
    ConsumerCreditReportConsumerReferralLocation
      <$> (o .:? "bureauCode")
      <*> (o .:? "bureauName")
      <*> (o .:? "address")
      <*> (o .:? "telephoneNumber")

-- | ToJSON ConsumerCreditReportConsumerReferralLocation
instance A.ToJSON ConsumerCreditReportConsumerReferralLocation where
  toJSON ConsumerCreditReportConsumerReferralLocation {..} =
    _omitNulls
      [ "bureauCode" .= consumerCreditReportConsumerReferralLocationBureauCode,
        "bureauName" .= consumerCreditReportConsumerReferralLocationBureauName,
        "address" .= consumerCreditReportConsumerReferralLocationAddress,
        "telephoneNumber" .= consumerCreditReportConsumerReferralLocationTelephoneNumber
      ]

-- | Construct a value of type 'ConsumerCreditReportConsumerReferralLocation' (by applying it's required fields, if any)
mkConsumerCreditReportConsumerReferralLocation ::
  ConsumerCreditReportConsumerReferralLocation
mkConsumerCreditReportConsumerReferralLocation =
  ConsumerCreditReportConsumerReferralLocation
    { consumerCreditReportConsumerReferralLocationBureauCode = Nothing,
      consumerCreditReportConsumerReferralLocationBureauName = Nothing,
      consumerCreditReportConsumerReferralLocationAddress = Nothing,
      consumerCreditReportConsumerReferralLocationTelephoneNumber = Nothing
    }

-- ** ConsumerCreditReportConsumerReferralLocationAddress

-- | ConsumerCreditReportConsumerReferralLocationAddress
data ConsumerCreditReportConsumerReferralLocationAddress = ConsumerCreditReportConsumerReferralLocationAddress
  { -- | "primaryAddress" - Referral Bureau primary address
    consumerCreditReportConsumerReferralLocationAddressPrimaryAddress :: !(Maybe Text),
    -- | "secondaryAddress" - Referral Bureau secondary address
    consumerCreditReportConsumerReferralLocationAddressSecondaryAddress :: !(Maybe Text),
    -- | "cityName" - City name
    consumerCreditReportConsumerReferralLocationAddressCityName :: !(Maybe Text),
    -- | "stateAbbreviation" - State name&#39;s abbreviation
    consumerCreditReportConsumerReferralLocationAddressStateAbbreviation :: !(Maybe Text),
    -- | "zipCode" - Zip code
    consumerCreditReportConsumerReferralLocationAddressZipCode :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportConsumerReferralLocationAddress
instance A.FromJSON ConsumerCreditReportConsumerReferralLocationAddress where
  parseJSON = A.withObject "ConsumerCreditReportConsumerReferralLocationAddress" $ \o ->
    ConsumerCreditReportConsumerReferralLocationAddress
      <$> (o .:? "primaryAddress")
      <*> (o .:? "secondaryAddress")
      <*> (o .:? "cityName")
      <*> (o .:? "stateAbbreviation")
      <*> (o .:? "zipCode")

-- | ToJSON ConsumerCreditReportConsumerReferralLocationAddress
instance A.ToJSON ConsumerCreditReportConsumerReferralLocationAddress where
  toJSON ConsumerCreditReportConsumerReferralLocationAddress {..} =
    _omitNulls
      [ "primaryAddress" .= consumerCreditReportConsumerReferralLocationAddressPrimaryAddress,
        "secondaryAddress" .= consumerCreditReportConsumerReferralLocationAddressSecondaryAddress,
        "cityName" .= consumerCreditReportConsumerReferralLocationAddressCityName,
        "stateAbbreviation" .= consumerCreditReportConsumerReferralLocationAddressStateAbbreviation,
        "zipCode" .= consumerCreditReportConsumerReferralLocationAddressZipCode
      ]

-- | Construct a value of type 'ConsumerCreditReportConsumerReferralLocationAddress' (by applying it's required fields, if any)
mkConsumerCreditReportConsumerReferralLocationAddress ::
  ConsumerCreditReportConsumerReferralLocationAddress
mkConsumerCreditReportConsumerReferralLocationAddress =
  ConsumerCreditReportConsumerReferralLocationAddress
    { consumerCreditReportConsumerReferralLocationAddressPrimaryAddress = Nothing,
      consumerCreditReportConsumerReferralLocationAddressSecondaryAddress = Nothing,
      consumerCreditReportConsumerReferralLocationAddressCityName = Nothing,
      consumerCreditReportConsumerReferralLocationAddressStateAbbreviation = Nothing,
      consumerCreditReportConsumerReferralLocationAddressZipCode = Nothing
    }

-- ** ConsumerCreditReportConsumerReferralLocationTelephoneNumber

-- | ConsumerCreditReportConsumerReferralLocationTelephoneNumber
-- Telephone number including a valid area code
newtype ConsumerCreditReportConsumerReferralLocationTelephoneNumber = ConsumerCreditReportConsumerReferralLocationTelephoneNumber
  { -- | "TelephoneNumber"
    consumerCreditReportConsumerReferralLocationTelephoneNumberTelephoneNumber :: Maybe Text
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportConsumerReferralLocationTelephoneNumber
instance A.FromJSON ConsumerCreditReportConsumerReferralLocationTelephoneNumber where
  parseJSON = A.withObject "ConsumerCreditReportConsumerReferralLocationTelephoneNumber" $ \o ->
    ConsumerCreditReportConsumerReferralLocationTelephoneNumber
      <$> (o .:? "TelephoneNumber")

-- | ToJSON ConsumerCreditReportConsumerReferralLocationTelephoneNumber
instance A.ToJSON ConsumerCreditReportConsumerReferralLocationTelephoneNumber where
  toJSON ConsumerCreditReportConsumerReferralLocationTelephoneNumber {..} =
    _omitNulls
      [ "TelephoneNumber" .= consumerCreditReportConsumerReferralLocationTelephoneNumberTelephoneNumber
      ]

-- | Construct a value of type 'ConsumerCreditReportConsumerReferralLocationTelephoneNumber' (by applying it's required fields, if any)
mkConsumerCreditReportConsumerReferralLocationTelephoneNumber ::
  ConsumerCreditReportConsumerReferralLocationTelephoneNumber
mkConsumerCreditReportConsumerReferralLocationTelephoneNumber =
  ConsumerCreditReportConsumerReferralLocationTelephoneNumber
    { consumerCreditReportConsumerReferralLocationTelephoneNumberTelephoneNumber = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSources

-- | ConsumerCreditReportAlternateDataSources
-- Contains the output data for the Optional Alternate Data Sources. These Alternate Data Sources are: - Military Lending Covered Borrower Status - North American Link - FraudIQ® Synthetic ID Alert
data ConsumerCreditReportAlternateDataSources = ConsumerCreditReportAlternateDataSources
  { -- | "alternateDataSourceErrorMessage" - It&#39;s returned whenever it encounters a communication problem or an error with the request of an Alternate Data Source. A separate \&quot;DERR\&quot; element will be returned per Data Source.  DERR: Alternate Data Source Message Response   CODE DESCRIPTION:   - 2001: COMMUNICATION ERROR   - 2002: COMMUNICATION ERROR   - 2003: COMMUNICATION ERROR   - 2004: COMMUNICATION ERROR   - 2005: COMMUNICATION ERROR   - 2006: COMMUNICATION ERROR   - 2007: COMMUNICATION ERROR   - 2008: COMMUNICATION ERROR   - 2009: COMMUNICATION ERROR   - 2010: COMMUNICATION ERROR   - 2011: COMMUNICATION ERROR   - 2012: COMMUNICATION ERROR   - 2013: COMMUNICATION ERROR   - 2014: COMMUNICATION ERROR   - 2015: COMMUNICATION ERROR   - 2016: COMMUNICATION ERROR   - 2017: COMMUNICATION ERROR   - 2018: COMMUNICATION ERROR   - 2019: COMMUNICATION ERROR   - 2020: COMMUNICATION ERROR   - 2021: COMMUNICATION ERROR   - 2022: COMMUNICATION ERROR   - 2023: COMMUNICATION ERROR   - 2024: COMMUNICATION ERROR   - 2025: COMMUNICATION ERROR
    consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage :: !(Maybe [ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage]),
    -- | "militaryLendingCoveredBorrower"
    consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower :: !(Maybe ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower),
    -- | "northAmericanLink"
    consumerCreditReportAlternateDataSourcesNorthAmericanLink :: !(Maybe ConsumerCreditReportAlternateDataSourcesNorthAmericanLink),
    -- | "fraudIQSyntheticIDAlerts"
    consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdAlerts :: !(Maybe ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts),
    -- | "fraudIQSyntheticIDV2Alerts"
    consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdv2Alerts :: !(Maybe ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSources
instance A.FromJSON ConsumerCreditReportAlternateDataSources where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSources" $ \o ->
    ConsumerCreditReportAlternateDataSources
      <$> (o .:? "alternateDataSourceErrorMessage")
      <*> (o .:? "militaryLendingCoveredBorrower")
      <*> (o .:? "northAmericanLink")
      <*> (o .:? "fraudIQSyntheticIDAlerts")
      <*> (o .:? "fraudIQSyntheticIDV2Alerts")

-- | ToJSON ConsumerCreditReportAlternateDataSources
instance A.ToJSON ConsumerCreditReportAlternateDataSources where
  toJSON ConsumerCreditReportAlternateDataSources {..} =
    _omitNulls
      [ "alternateDataSourceErrorMessage" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage,
        "militaryLendingCoveredBorrower" .= consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower,
        "northAmericanLink" .= consumerCreditReportAlternateDataSourcesNorthAmericanLink,
        "fraudIQSyntheticIDAlerts" .= consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdAlerts,
        "fraudIQSyntheticIDV2Alerts" .= consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdv2Alerts
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSources' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSources ::
  ConsumerCreditReportAlternateDataSources
mkConsumerCreditReportAlternateDataSources =
  ConsumerCreditReportAlternateDataSources
    { consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage = Nothing,
      consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower = Nothing,
      consumerCreditReportAlternateDataSourcesNorthAmericanLink = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdAlerts = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIqSyntheticIdv2Alerts = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode

-- | ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
data ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode = ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
  { -- | "code" - Code value
    consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeCode :: !(Maybe Text),
    -- | "description" - Description for the given code
    consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeDescription :: !(Maybe Text),
    -- | "errorCodes" - Container for all the errors returned by the Alternate Data Sources
    consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeErrorCodes :: !(Maybe [ConsumerCreditReportAlternateDataSourcesErrorCodes])
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode" $ \o ->
    ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
      <$> (o .:? "code")
      <*> (o .:? "description")
      <*> (o .:? "errorCodes")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode where
  toJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeCode,
        "description" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeDescription,
        "errorCodes" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeErrorCodes
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode ::
  ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
mkConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode =
  ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode
    { consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeCode = Nothing,
      consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeDescription = Nothing,
      consumerCreditReportAlternateDataSourcesAlternateDataSourceCodeErrorCodes = Nothing
    }

-- ** ConsumerCreditReportTypeCode

-- | ConsumerCreditReportTypeCode
-- The type of identification
data ConsumerCreditReportTypeCode = ConsumerCreditReportTypeCode
  { -- | "code" - Code value
    consumerCreditReportTypeCodeCode :: !(Maybe Text),
    -- | "description" -  - C: S.I.N. - S:  SSN - T: TAX ID
    consumerCreditReportTypeCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportTypeCode
instance A.FromJSON ConsumerCreditReportTypeCode where
  parseJSON = A.withObject "ConsumerCreditReportTypeCode" $ \o ->
    ConsumerCreditReportTypeCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportTypeCode
instance A.ToJSON ConsumerCreditReportTypeCode where
  toJSON ConsumerCreditReportTypeCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportTypeCodeCode,
        "description" .= consumerCreditReportTypeCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportTypeCode' (by applying it's required fields, if any)
mkConsumerCreditReportTypeCode ::
  ConsumerCreditReportTypeCode
mkConsumerCreditReportTypeCode =
  ConsumerCreditReportTypeCode
    { consumerCreditReportTypeCodeCode = Nothing,
      consumerCreditReportTypeCodeDescription = Nothing
    }

-- ** ConsumerCreditReportReasonCode

-- | ConsumerCreditReportReasonCode
-- Reason code
data ConsumerCreditReportReasonCode = ConsumerCreditReportReasonCode
  { -- | "code" - Code value
    consumerCreditReportReasonCodeCode :: !(Maybe Text),
    -- | "description" - Reason codes - B: Tax ID Number - C: Minor child - E: S.I.N. (Canadian Social Insurance Number) - V: Variation
    consumerCreditReportReasonCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportReasonCode
instance A.FromJSON ConsumerCreditReportReasonCode where
  parseJSON = A.withObject "ConsumerCreditReportReasonCode" $ \o ->
    ConsumerCreditReportReasonCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportReasonCode
instance A.ToJSON ConsumerCreditReportReasonCode where
  toJSON ConsumerCreditReportReasonCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportReasonCodeCode,
        "description" .= consumerCreditReportReasonCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportReasonCode' (by applying it's required fields, if any)
mkConsumerCreditReportReasonCode ::
  ConsumerCreditReportReasonCode
mkConsumerCreditReportReasonCode =
  ConsumerCreditReportReasonCode
    { consumerCreditReportReasonCodeCode = Nothing,
      consumerCreditReportReasonCodeDescription = Nothing
    }

-- ** ConsumerCreditReportFICOScoreIndicatorCode

-- | ConsumerCreditReportFICOScoreIndicatorCode
-- FICO Score Indicator
data ConsumerCreditReportFICOScoreIndicatorCode = ConsumerCreditReportFICOScoreIndicatorCode
  { -- | "code" - Code value
    consumerCreditReportFICOScoreIndicatorCodeCode :: !(Maybe Text),
    -- | "description" - NF &#x3D; Non‐FACTA; F &#x3D; FACTA version   - S: FICO® Score 9 based on Equifax Data (NF)   - 4: FICO® Auto Score 9 based on Equifax Data (NF)   - 8: FICO® Bankcard Score 9 based on Equifax Data (NF)   - V: FICO® Risk Score 9 based on Equifax Data (F)   - @: FICO® Auto Score 9 based on Equifax Data (F)   - $: FICO® Bankcard Score 9 based on Equifax Data (F)   - 9: FICO® Score 8 based on Equifax Data (NF)   - C: FICO® Auto Score 8 based on Equifax Data (NF)   - D: FICO® Bankcard Score 8 based on Equifax Data (NF)   - O: FICO® Mortgage Score 8 based on Equifax Data (NF)   - G: FICO® Score 8 based on Equifax Data (F)   - H: FICO® Auto Score 8 based on Equifax Data (F)   - X: FICO® Bankcard Score 8 based on Equifax Data (F)   - Q: FICO® Mortgage Score 8 based on Equifax Data (F)   - J: FICO® Score 5 based on Equifax Data (NF)   - K: FICO® Auto Score 5 based on Equifax Data (NF)   - L: FICO® Bankcard Score 5 based on Equifax Data (NF)   - M: FICO® Personal Finance Score 5 based on Equifax Data (NF)   - N: FICO® Installment Score 5 based on Equifax Data (NF)   - Z: FICO® Score 5 based on Equifax Data (F)   - R: FICO® Auto Score 5 based on Equifax Data (F)   - W: FICO® Bankcard Score 5 based on Equifax Data (F)   - U: FICO® Personal Finance Score 5 based on Equifax Data (F)   - P: FICO® Installment Score 5 based on Equifax Data (F)
    consumerCreditReportFICOScoreIndicatorCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportFICOScoreIndicatorCode
instance A.FromJSON ConsumerCreditReportFICOScoreIndicatorCode where
  parseJSON = A.withObject "ConsumerCreditReportFICOScoreIndicatorCode" $ \o ->
    ConsumerCreditReportFICOScoreIndicatorCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportFICOScoreIndicatorCode
instance A.ToJSON ConsumerCreditReportFICOScoreIndicatorCode where
  toJSON ConsumerCreditReportFICOScoreIndicatorCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportFICOScoreIndicatorCodeCode,
        "description" .= consumerCreditReportFICOScoreIndicatorCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportFICOScoreIndicatorCode' (by applying it's required fields, if any)
mkConsumerCreditReportFICOScoreIndicatorCode ::
  ConsumerCreditReportFICOScoreIndicatorCode
mkConsumerCreditReportFICOScoreIndicatorCode =
  ConsumerCreditReportFICOScoreIndicatorCode
    { consumerCreditReportFICOScoreIndicatorCodeCode = Nothing,
      consumerCreditReportFICOScoreIndicatorCodeDescription = Nothing
    }

-- ** ConsumerCreditReportReasons

-- | ConsumerCreditReportReasons
newtype ConsumerCreditReportReasons = ConsumerCreditReportReasons
  { -- | "code" - Code value
    consumerCreditReportReasonsCode :: Maybe Text
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportReasons
instance A.FromJSON ConsumerCreditReportReasons where
  parseJSON = A.withObject "ConsumerCreditReportReasons" $ \o ->
    ConsumerCreditReportReasons
      <$> (o .:? "code")

-- | ToJSON ConsumerCreditReportReasons
instance A.ToJSON ConsumerCreditReportReasons where
  toJSON ConsumerCreditReportReasons {..} =
    _omitNulls
      [ "code" .= consumerCreditReportReasonsCode
      ]

-- | Construct a value of type 'ConsumerCreditReportReasons' (by applying it's required fields, if any)
mkConsumerCreditReportReasons ::
  ConsumerCreditReportReasons
mkConsumerCreditReportReasons =
  ConsumerCreditReportReasons
    { consumerCreditReportReasonsCode = Nothing
    }

-- ** ConsumerCreditReportInquiryKeyFactor

-- | ConsumerCreditReportInquiryKeyFactor
data ConsumerCreditReportInquiryKeyFactor = ConsumerCreditReportInquiryKeyFactor
  { -- | "code"
    consumerCreditReportInquiryKeyFactorCode :: !(Maybe Text),
    -- | "description" - FACTA 5th Reason Code Regarding Inquiries
    consumerCreditReportInquiryKeyFactorDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportInquiryKeyFactor
instance A.FromJSON ConsumerCreditReportInquiryKeyFactor where
  parseJSON = A.withObject "ConsumerCreditReportInquiryKeyFactor" $ \o ->
    ConsumerCreditReportInquiryKeyFactor
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportInquiryKeyFactor
instance A.ToJSON ConsumerCreditReportInquiryKeyFactor where
  toJSON ConsumerCreditReportInquiryKeyFactor {..} =
    _omitNulls
      [ "code" .= consumerCreditReportInquiryKeyFactorCode,
        "description" .= consumerCreditReportInquiryKeyFactorDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportInquiryKeyFactor' (by applying it's required fields, if any)
mkConsumerCreditReportInquiryKeyFactor ::
  ConsumerCreditReportInquiryKeyFactor
mkConsumerCreditReportInquiryKeyFactor =
  ConsumerCreditReportInquiryKeyFactor
    { consumerCreditReportInquiryKeyFactorCode = Nothing,
      consumerCreditReportInquiryKeyFactorDescription = Nothing
    }

-- ** ConsumerCreditReportRiskBasedPricingOrModel

-- | ConsumerCreditReportRiskBasedPricingOrModel
data ConsumerCreditReportRiskBasedPricingOrModel = ConsumerCreditReportRiskBasedPricingOrModel
  { -- | "percentage" - Percentage of the U.S. population that the Consumers Score for this model scores
    consumerCreditReportRiskBasedPricingOrModelPercentage :: !(Maybe Text),
    -- | "lowRange" - Dodd Frank Wall Street Lowest Score available on the model requested
    consumerCreditReportRiskBasedPricingOrModelLowRange :: !(Maybe Text),
    -- | "highRange" - Dodd Frank Wall Street Highest Score available on the model requested
    consumerCreditReportRiskBasedPricingOrModelHighRange :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportRiskBasedPricingOrModel
instance A.FromJSON ConsumerCreditReportRiskBasedPricingOrModel where
  parseJSON = A.withObject "ConsumerCreditReportRiskBasedPricingOrModel" $ \o ->
    ConsumerCreditReportRiskBasedPricingOrModel
      <$> (o .:? "percentage")
      <*> (o .:? "lowRange")
      <*> (o .:? "highRange")

-- | ToJSON ConsumerCreditReportRiskBasedPricingOrModel
instance A.ToJSON ConsumerCreditReportRiskBasedPricingOrModel where
  toJSON ConsumerCreditReportRiskBasedPricingOrModel {..} =
    _omitNulls
      [ "percentage" .= consumerCreditReportRiskBasedPricingOrModelPercentage,
        "lowRange" .= consumerCreditReportRiskBasedPricingOrModelLowRange,
        "highRange" .= consumerCreditReportRiskBasedPricingOrModelHighRange
      ]

-- | Construct a value of type 'ConsumerCreditReportRiskBasedPricingOrModel' (by applying it's required fields, if any)
mkConsumerCreditReportRiskBasedPricingOrModel ::
  ConsumerCreditReportRiskBasedPricingOrModel
mkConsumerCreditReportRiskBasedPricingOrModel =
  ConsumerCreditReportRiskBasedPricingOrModel
    { consumerCreditReportRiskBasedPricingOrModelPercentage = Nothing,
      consumerCreditReportRiskBasedPricingOrModelLowRange = Nothing,
      consumerCreditReportRiskBasedPricingOrModelHighRange = Nothing
    }

-- ** ConsumerCreditReportRejects

-- | ConsumerCreditReportRejects
-- Reject codes corresponding to the model
data ConsumerCreditReportRejects = ConsumerCreditReportRejects
  { -- | "code" - Code value
    consumerCreditReportRejectsCode :: !(Maybe Text),
    -- | "description" - Reject Codes corresponding to Model Number 02781   - A1: File Under Review   - B0: Insufficient Information to Score   - D0: Bankruptcy On File   - L0: Subject Deceased   - N1: Inquiry Only File   - X7: File Cannot Be Scored    Reject Codes corresponding to Model Number 02782   - A1: File Under Review   - B0: Insufficient Information to Score   - D0: Bankruptcy On File   - L0: Subject Deceased   - N1: Inquiry Only File   - X7: File Cannot Be Scored    Reject Codes corresponding to Model Number 02783   - A1: File Under Review   - B0: Insufficient Information to Score   - D0: Bankruptcy On File   - L0: Subject Deceased   - N1: Inquiry Only File   - X7: File Cannot Be Scored    Reject Codes corresponding to Model Number 02784   - A1: File Under Review   - B0: Insufficient Information to Score   - D0: Bankruptcy On File   - L0: Subject Deceased   - N1: Inquiry Only File   - X7: File Cannot Be Scored    Reject Codes corresponding to Model Number 05146   - A1: File Under Review   - C1: No Qualifying Accounts Present   - C2: Bankruptcy Occurrence Present   - L0: Consumer Deceased   - R0: Insufficient Information to Score   - X1: Model Delivery Temporarily Unavailable   - X3: Too Many Inquiries on File   - X5: No Input Fields Provided   - X7: Model Delivery Temporarily Unavailable, File Cannot Be Scored    Reject Codes corresponding to Model Number 05147   - A1: File Under Review   - C1: No Qualifying Accounts Present   - C2: Bankruptcy Occurrence Present   - L0: Consumer Deceased   - R0: Insufficient Information to Score   - X1: Model Delivery Temporarily Unavailable   - X3: Too Many Inquiries on File   - X5: No Input Fields Provided   - X7: Model Delivery Temporarily Unavailable, File Cannot Be Scored    Reject Codes corresponding to Model Number 05276   - A1: File Under Review   - C1: No Qualifying Accounts Present   - C2: Bankruptcy Occurrence Present   - L0: Consumer Deceased   - R0: Insufficient Information to Score   - X1: Model Delivery Temporarily Unavailable   - X3: Too Many Inquiries on File   - X5: No Input Fields Provided   - X7: Model Delivery Temporarily Unavailable, File Cannot Be Scored  Reject Codes corresponding to Model Number 05277   - A1: File Under Review   - C1: No Qualifying Accounts Present   - C2: Bankruptcy Occurrence Present   - L0: Consumer Deceased   - R0: Insufficient Information to Score   - X1: Model Delivery Temporarily Unavailable   - X3: Too Many Inquiries on File   - X5: No Input Fields Provided   - X7: Model Delivery Temporarily Unavailable, File Cannot Be Scored    Reject Codes corresponding to Model Number 02502   - B0: No trade or inquiry reported within the last 24 months   - B1: Subject Deceased   - D0: Bankruptcy on File   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02503   - B0: No trade or inquiry reported within the last 24 months   - B1: Subject Deceased   - D0: Bankruptcy on File   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02037   - B0: No trade or inquiry reported within 24 months   - B1: Subject deceased   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02525   - B0: No trade or inquiry reported within 24 months   - B1: Subject deceased   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02725   - B0: No trade or inquiry reported within 24 months   - B1: Subject deceased   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02874   - B0: Model Delivery is Not Available: Insufficient Information to Score   - L0: Model Delivery is Not Available: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available: File Under Review   - F1: Model Delivery is Not Available: No Qualifying Accounts Present    Reject Codes corresponding to Model Number 02875   - B0: Model Delivery is Not Available: Insufficient Information to Score   - L0: Model Delivery is Not Available: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available: File Under Review   - F1: Model Delivery is Not Available: No Qualifying Accounts Present    Reject Codes corresponding to Model Number 02893   - B0: Model Delivery is Not Available: Insufficient Information to Score   - L0: Model Delivery is Not Available: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available: File Under Review   - F1: Model Delivery is Not Available: No Qualifying Accounts Present    Reject Codes corresponding to Model Number 02481   - B0: No trade or inquiry reported within 24 months   - L0: Subject Deceased   - L1: No qualifying information present   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02527   - B0: No trade or inquiry reported within 24 months   - L0: Subject Deceased   - L1: No qualifying information present   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02575   - B0: No trade or inquiry reported within 24 months   - L0: Subject Deceased   - X7: File cannot be scored   - A1: File Under Review    Reject Codes corresponding to Model Number 02531   - P1: Subject deceased   - P2: No qualifying trades   - P3: Insufficient or unknown tradeline history   - P4: No recently reported tradelines   - P5: No recent trades w/o data suppression or disputed info   - P6: Unable to score   - X7: Model delivery is temporarily unavailable    Reject Codes corresponding to Model Number 02825   - P1: Subject deceased   - P2: No qualifying trades   - P3: Insufficient or unknown tradeline history   - P4: No recently reported tradelines   - P5: No recent trades w/o data suppression or disputed info   - P6: Unable to score   - X7: Model delivery is temporarily unavailable    Reject Codes corresponding to Model Number 02905   - P1: Subject deceased   - P2: No qualifying trades   - P3: Insufficient or unknown tradeline history   - P4: No recently reported tradelines   - P5: No recent trades w/o data suppression or disputed info   - P6: Unable to score   - X7: Model delivery is temporarily unavailable    Reject Codes corresponding to Model Number 02906   - P1: Subject deceased   - P2: No qualifying trades   - P3: Insufficient or unknown tradeline history   - P4: No recently reported tradelines   - P5: No recent trades w/o data suppression or disputed info   - P6: Unable to score   - X7: Model delivery is temporarily unavailable    Reject Codes corresponding to Model Number 02682   - A1: File Under Review   - B0: Insufficient Information to Score   - L0: Subject deceased   - X7: File Cannot Be Scored    Reject Codes corresponding to Model Number 02718   - A1: File Under Review   - B0: Insufficient Information to Score   - L0: Subject deceased   - X7: File Cannot Be Scored   - N1: Inquiry Only File    Reject Codes corresponding to Model Number 02485   - B0: No trade or inquiry reported within the last 24 months   - X7: File cannot be scored    Reject Codes corresponding to Model Number 02505   - B1: Subject Deceased   - F1: No qualifying accounts present   - F3: No recently reported account information   - A0: Not available, file cannot be scored    Reject Codes corresponding to Model Number 02558   - B1: Subject Deceased   - F1: No qualifying accounts present   - F3: No recently reported account information   - A0: Not available, file cannot be scored    Reject Codes corresponding to Model Number 01203   - A1: File Under Review   - B0: Insufficient Information to Score   - F1: No Qualifying Accounts Present   - L0: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable    Reject Codes corresponding to Model Number 01204   - A1: File Under Review   - B0: Insufficient Information to Score   - F1: No Qualifying Accounts Present   - L0: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable  Reject Codes corresponding to Model Number 02912   - A1: File Under Review   - B0: Insufficient Information to Score   - F1: No Qualifying Accounts Present   - L0: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable  Reject Codes corresponding to Model Number 02903   - A1: File Under Review   - B0: Insufficient Information to Score   - F1: No Qualifying Accounts Present   - L0: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable  Reject Codes corresponding to Model Number 02904   - A1: File Under Review   - B0: Insufficient Information to Score   - F1: No Qualifying Accounts Present   - L0: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable  Reject Codes corresponding to Model Number 02470   - B0: No trade or inquiry reported within 24 months   - B1: Subject deceased   - X7: File cannot be scored  Reject Codes corresponding to Model Number 02557   - B0: No trade or inquiry reported within 24 months   - B1: Subject deceased   - X7: File cannot be scored  Reject Codes corresponding to Model Number 02458   - B0: No trade or inquiry reported within 24 months   - B1: Subject deceased   - X7: File cannot be scored  Reject Codes corresponding to Model Number 02479   - B0: No trade or inquiry reported within the last 24 months   - B1: Subject Deceased   - D0: Bankruptcy on file   - X7: File cannot be scored  Reject Codes corresponding to Model Number 02978   - L0: Deceased   - F1: Lack of Trades   - B0: Lack of Activity   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available  Reject Codes corresponding to Model Number 02991   - L0: Deceased   - F1: Lack of Trades   - B0: Lack of Activity   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available  Reject Codes corresponding to Model Number 05008   - L0: Deceased   - F1: Lack of Trades   - B0: Lack of Activity   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available  Reject Codes corresponding to Model Number 05028   - L0: Deceased   - F1: Lack of Trades   - B0: Lack of Activity   - X7: Model Delivery is Temporarily Unavailable   - A1: Model Delivery is Not Available  Reject Codes corresponding to Model Number 05143   - 000L0: Deceased   - 000F1: Lack of Trades   - 000B0: Lack of Activity   - 000X7: Model Delivery is Temporarily Unavailable   - 000A1: Model Delivery is Not Available  Reject Codes corresponding to Model Number 05151   - 000L0: Deceased   - 000F1: Lack of Trades   - 000B0: Lack of Activity   - 000X7: Model Delivery is Temporarily Unavailable   - 000A1: Model Delivery is Not Available  Reject Codes corresponding to Model Number 02770   - B0: Insufficient Information to Score   - L0: Subject Deceased   - X7: Model Delivery is Temporarily Unavailable   - A1: File Under Review  Reject Codes corresponding to Model Number 02778   - B1: Subject deceased   - F1: No trades on file   - F2: No trades on file with date open older than 5 months   - F3: No trades on files with date reported in last 6 months   - F4: No trades on file with a valid current status and without data suppression or disputed info   - X5: Invalid source data input (request missing a 1, 2, 3, 4 or 5 for the score desired)  Reject Codes corresponding to Model Number 02779   - B1: Subject deceased   - F1: No trades on file   - F2: No trades on file with date open older than 5 months   - F3: No trades on files with date reported in last 6 months   - F4: No trades on file with a valid current status and without data suppression or disputed info   - X5: Invalid source data input (request missing a 1, 2, 3, 4 or 5 for the score desired)  Reject Codes corresponding to Model Number 01958   - B0: No Trade/Inq     - B1: Deceased  Reject Codes corresponding to Model Number 01918   - B0: No Trade/Inq     - L0: Deceased  Reject Codes corresponding to Model Number 05236   - 01: DECEASED ON CREDIT, NCTUE AND LEXISNEXIS   - 02: DECEASED ON CREDIT AND NCTUE   - 03: DECEASED ON CREDIT AND LEXISNEXIS   - 04: DECEASED ON NCTUE AND LEXISNEXIS   - 05: DECEASED ON CREDIT   - 06: DECEASED ON NCTUE   - 07: DECEASED ON LEXISNEXIS   - 08: FILE IN REVIEW-CREDIT,FRAUD-NCTUE,IDENTITY THEFT -LEXISNEXIS   - 09: FILE IN REVIEW-CREDIT,FRAUD-NCTUE   - 10: FILE IN REVIEW-CREDIT,IDENTITY THEFT -LEXISNEXIS   - 11: FRAUD-NCTUE,IDENTITY THEFT -LEXISNEXIS   - 12: FILE IN REVIEW-CREDIT   - 13: FRAUD-NCTUE   - 14: IDENTITY THEFT -LEXISNEXIS   - 15: NO HIT-CREDIT,NO HIT-NCTUE,NO HIT-LEXISNEXIS   - 16: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,NO HIT-LEXISNEXIS   - 17: NO HIT-CREDIT,INQUIRY ONLY-NCTUE,NO HIT-LEXISNEXIS   - 18: INADEQUATE INFO-CREDIT,INQUIRY ONLY-NCTUE,NO HIT-LEXISNEXIS   - 19: NO HIT-CREDIT,BANKRUPTCY-NCTUE,NO HIT-LEXISNEXIS   - 20: INADEQUATE INFO-CREDIT,BANKRUPTCY-NCTUE,NO HIT-LEXISNEXIS   - 21: NO HIT-CREDIT,NO HIT-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 22: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 23: NO HIT-CREDIT,INQ ONLY-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 24: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 25: NO HIT-CREDIT,BKP-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 26: INADEQUATE INFO-CREDIT,BKP-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 27: NO HIT-CREDIT,NO HIT-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 28: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 29: NO HIT-CREDIT,INQ ONLY-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 30: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 31: NO HIT-CREDIT,BKP-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 32: INADEQUATE INFO-CREDIT,BKP-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 33: NO HIT-CREDIT,NO HIT-NCTUE,SECURITY ALERT-LEXISNEXIS   - 34: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,SECURITY ALERT-LEXISNEXIS   - 35: NO HIT-CREDIT,INQ ONLY-NCTUE,SECURITY ALERT-LEXISNEXIS   - 36: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,SECURITY ALERT-LEXISNEXIS   - 37: NO HIT-CREDIT,BKP-NCTUE,SECURITY ALERT-LEXISNEXIS   - 38: INADEQUATE INFO-CREDIT,BKP-NCTUE,SECURITY ALERT-LEXISNEXIS   - 39: NO HIT-CREDIT,NO HIT-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 40: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 41: NO HIT-CREDIT,INQ ONLY-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 42: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 43: NO HIT-CREDIT,BKP-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 44: INADEQUATE INFO-CREDIT,BKP-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 45: NO HIT-CREDIT,NO HIT-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 46: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 47: NO HIT-CREDIT,INQ ONLY-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 48: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 49: NO HIT-CREDIT,BKP-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 50: INADEQUATE INFO-CREDIT,BKP-NCTUE,INADEQUATE INFO-LEXISNEXIS   - A1: **NOT AVAILABLE, FILE UNDER REVIEW   - B0: INSUFFICIENT INFORMATION TO SCORE   - C1: MODEL DELIVERY IS NOT AVAILABLE: NO QUALIFYING ACCOUNTS PRESENT. NO HIT   - C2: MODEL DELIVERY UNAVAILABLE: FILE CONDITION NOT MET   - C3: ADDRESS FORMATTED INCORRECTLY FOR ALTERNATE DATA SOURCE   - C4: NAME FORMATTED INCORRECTLY FOR ALTERNATE DATA SOURCE   - C5: BUSINESS NAME INVALID   - C6: XML FORMAT ERROR BETWEEN INTERNAL DATABASES    - C7: INVALID ALTERNATE DATA SOURCE MBR NBR ON DATABASE SERVER   - C8: ALTERNATE DATA SOURCE MBR NBR NOT AUTHORIZED ON DATABASE   - C9: INVALID SECURITY CODE FOR ALTERNATE DATA SRCE ON DATABASE   - CC: NOT AUTHORIZED TO RECEIVE INSIGHT SCORES   - CD: INVALID PRODUCT CONFIGURATION FOR INSIGHT SCORES   - CE: ALTERNATE DATA SOURCE ERROR - MAX OF 4 ADDRESSES ALLOWED   - CF: INVALID ONLINE MATCH CONFIGURATION FOR INSIGHT SCORE   - CG: ALTERNATE DATA SRC ERROR - NO CONSUMER SRCH FIELDS PROVIDED   - CH: MISSING REQUIRED FIELDS FOR INTERNAL DATABASE SEARCH   - CJ: INTERNAL DATABASE ERROR - PLEASE TRY LATER OR CONTACT EFX   - CK: INTERNAL MDB LAYOUT ERROR   - CL: RETRY TRANSACTION TRANSIENT ERROR   - CQ: INTERNAL PROCESSING ERROR PLEASE TRY LATER OR CONTACT EFX   - CS: MODEL SERVER ERROR PLEASE TRY AGAIN LATER OR CONTACT EFX   - CT: INTERNAL CUSTOMER ID ERROR   - CX: HTTP ERROR RETURNED TO OMS FROM NC+/CONNEXUS   - CZ: RETRY TRANSACTION - IF ERROR CONTINUES, CONTACT EQUIFAX   - L0: MODEL DELIVERY IS NOT AVAILABLE: SUBJECT DECEASED   - N1: INQUIRY ONLY FILE   - R0: MODEL DELIVERY IS NOT AVAILABLE: INSUFFICIENT INFORMATION TO SCORE   - X7: MODEL DELIVERY IS TEMPORARILY UNAVAILABLE  Reject Codes corresponding to Model Number 05168   - B: RISKWISE AUTHORIZATION REQUIRED - CONTACT YOUR SALES REP   - R: DATA SOURCE CURRENTLY UNAVAILABLE   - S: DATA SOURCE CURRENTLY UNAVAILABLE   - T: REQUESTED PRODUCT UNAVAILABLE, CONTACT SALES REP   - U: UNABLE TO ACCESS DATA SOURCE   - V: UNABLE TO ACCESS DATA SOURCE   - W: DATA SOURCE IS TEMPORARILY OUT OF SERVICE   - X: DATA SOURCE IS TEMPORARILY OUT OF SERVICE   - Y: REQUIRED DATA NOT PRESENT   - Z: REQUIRED DATA NOT PRESENT 04  Reject Codes corresponding to Model Number 05271   - A1: MODEL DELIVERY NOT AVAILABLE: FILE UNDER REVIEW   - C1: MODEL DELIVERY NOT AVAILABLE: NO QUALIFYING ACCOUNTS PRESENT. NO HIT   - L0: MODEL DELIVERY NOT AVAILABLE: SUBJECT DECEASED   - R0: MODEL DELIVERY NOT AVAILABLE: INSUFFICIENT INFORMATION TO SCORE  Reject Codes corresponding to Model Number 05274   - 01: DECEASED ON CREDIT, NCTUE AND LEXISNEXIS   - 02: DECEASED ON CREDIT AND NCTUE   - 03: DECEASED ON CREDIT AND LEXISNEXIS   - 04: DECEASED ON NCTUE AND LEXISNEXIS   - 05: DECEASED ON CREDIT   - 06: DECEASED ON NCTUE   - 07: DECEASED ON LEXISNEXIS   - 08: FILE IN REVIEW-CREDIT,FRAUD-NCTUE,IDENTITY THEFT -LEXISNEXIS   - 09: FILE IN REVIEW-CREDIT,FRAUD-NCTUE   - 10: FILE IN REVIEW-CREDIT,IDENTITY THEFT -LEXISNEXIS   - 11: FRAUD-NCTUE,IDENTITY THEFT -LEXISNEXIS   - 12: FILE IN REVIEW-CREDIT   - 13: FRAUD-NCTUE   - 14: IDENTITY THEFT -LEXISNEXIS   - 15: NO HIT-CREDIT,NO HIT-NCTUE,NO HIT-LEXISNEXIS   - 16: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,NO HIT-LEXISNEXIS   - 17: NO HIT-CREDIT,INQUIRY ONLY-NCTUE,NO HIT-LEXISNEXIS   - 18: INADEQUATE INFO-CREDIT,INQUIRY ONLY-NCTUE,NO HIT-LEXISNEXIS   - 19: NO HIT-CREDIT,BANKRUPTCY-NCTUE,NO HIT-LEXISNEXIS   - 20: INADEQUATE INFO-CREDIT,BANKRUPTCY-NCTUE,NO HIT-LEXISNEXIS   - 21: NO HIT-CREDIT,NO HIT-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 22: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 23: NO HIT-CREDIT,INQ ONLY-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 24: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 25: NO HIT-CREDIT,BKP-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 26: INADEQUATE INFO-CREDIT,BKP-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 27: NO HIT-CREDIT,NO HIT-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 28: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 29: NO HIT-CREDIT,INQ ONLY-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 30: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 31: NO HIT-CREDIT,BKP-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 32: INADEQUATE INFO-CREDIT,BKP-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 33: NO HIT-CREDIT,NO HIT-NCTUE,SECURITY ALERT-LEXISNEXIS   - 34: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,SECURITY ALERT-LEXISNEXIS   - 35: NO HIT-CREDIT,INQ ONLY-NCTUE,SECURITY ALERT-LEXISNEXIS   - 36: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,SECURITY ALERT-LEXISNEXIS   - 37: NO HIT-CREDIT,BKP-NCTUE,SECURITY ALERT-LEXISNEXIS   - 38: INADEQUATE INFO-CREDIT,BKP-NCTUE,SECURITY ALERT-LEXISNEXIS   - 39: NO HIT-CREDIT,NO HIT-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 40: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 41: NO HIT-CREDIT,INQ ONLY-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 42: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 43: NO HIT-CREDIT,BKP-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 44: INADEQUATE INFO-CREDIT,BKP-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 45: NO HIT-CREDIT,NO HIT-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 46: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 47: NO HIT-CREDIT,INQ ONLY-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 48: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 49: NO HIT-CREDIT,BKP-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 50: INADEQUATE INFO-CREDIT,BKP-NCTUE,INADEQUATE INFO-LEXISNEXIS   - A1: **NOT AVAILABLE, FILE UNDER REVIEW   - C1: MODEL DELIVERY IS NOT AVAILABLE: NO QUALIFYING ACCOUNTS PRESENT. NO HIT   - C2: MODEL DELIVERY UNAVAILABLE, FILE CONDITION NOT MET   - C3: ADDRESS FORMATTED INCORRECTLY FOR ALTERNATE DATA SOURCE   - C4: NAME FORMATTED INCORRECTLY FOR ALTERNATE DATA SOURCE   - C5: BUSINESS NAME INVALID   - C6: XML FORMAT ERROR BETWEEN INTERNAL DATABASES    - C7: INVALID ALTERNATE DATA SOURCE MBR NBR ON DATABASE SERVER   - C8: ALTERNATE DATA SOURCE MBR NBR NOT AUTHORIZED ON DATABASE   - C9: INVALID SECURITY CODE FOR ALTERNATE DATA SRCE ON DATABASE   - CC: NOT AUTHORIZED TO RECEIVE INSIGHT SCORES   - CD: INVALID PRODUCT CONFIGURATION FOR INSIGHT SCORES   - CE: ALTERNATE DATA SOURCE ERROR - MAX OF 4 ADDRESSES ALLOWED   - CF: INVALID ONLINE MATCH CONFIGURATION FOR INSIGHT SCORE   - CG: ALTERNATE DATA SRC ERROR - NO CONSUMER SRCH FIELDS PROVIDED   - CH: MISSING REQUIRED FIELDS FOR INTERNAL DATABASE SEARCH   - CJ: INTERNAL DATABASE ERROR - PLEASE TRY LATER OR CONTACT EFX   - CK: INTERNAL MDB LAYOUT ERROR   - CL: RETRY TRANSACTION TRANSIENT ERROR   - CQ: INTERNAL PROCESSING ERROR PLEASE TRY LATER OR CONTACT EFX   - CS: MODEL SERVER ERROR PLEASE TRY AGAIN LATER OR CONTACT EFX   - CT: INTERNAL CUSTOMER ID ERROR   - CX: HTTP ERROR RETURNED TO OMS FROM NC+/CONNEXUS   - CZ: RETRY TRANSACTION - IF ERROR CONTINUES, CONTACT EQUIFAX   - L0: MODEL DELIVERY IS NOT AVAILABLE: SUBJECT DECEASED   - R0: MODEL DELIVERY IS NOT AVAILABLE: INSUFFICIENT INFORMATION TO SCORE  Reject Codes corresponding to Model Number 05285   - A1: **NOT AVAILABLE, FILE UNDER REVIEW   - B0: INSUFFICIENT INFORMATION TO SCORE   - C1: MODEL DELIVERY IS NOT AVAILABLE: NO QUALIFYING ACCOUNTS PRESENT. NO HIT   - L0: MODEL DELIVERY IS NOT AVAILABLE: SUBJECT DECEASED   - N1: INQUIRY ONLY FILE   - R0: MODEL DELIVERY IS NOT AVAILABLE: INSUFFICIENT INFORMATION TO SCORE   - X7: MODEL DELIVERY IS TEMPORARILY UNAVAILABLE  Reject Codes corresponding to Model Number 05264   - 01: DECEASED ON CREDIT, NCTUE AND LEXISNEXIS   - 02: DECEASED ON CREDIT AND NCTUE   - 03: DECEASED ON CREDIT AND LEXISNEXIS   - 04: DECEASED ON NCTUE AND LEXISNEXIS   - 05: DECEASED ON CREDIT   - 06: DECEASED ON NCTUE   - 07: DECEASED ON LEXISNEXIS   - 08: FILE IN REVIEW-CREDIT,FRAUD-NCTUE,IDENTITY THEFT-LEXISNEXIS   - 09: FILE IN REVIEW-CREDIT,FRAUD-NCTUE   - 10: FILE IN REVIEW-CREDIT,IDENTITY THEFT -LEXISNEXIS   - 11: FRAUD-NCTUE,IDENTITY THEFT -LEXISNEXIS   - 12: FILE IN REVIEW-CREDIT   - 13: FRAUD-NCTUE   - 14: IDENTITY THEFT -LEXISNEXIS   - 15: NO HIT-CREDIT,NO HIT-NCTUE,NO HIT-LEXISNEXIS   - 16: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,NO HIT-LEXISNEXIS   - 17: NO HIT-CREDIT,INQUIRY ONLY-NCTUE,NO HIT-LEXISNEXIS   - 18: INADEQUATE INFO-CREDIT,INQUIRY ONLY-NCTUE,NO HIT-LEXISNEXIS   - 19: NO HIT-CREDIT,BANKRUPTCY-NCTUE,NO HIT-LEXISNEXIS   - 20: INADEQUATE INFO-CREDIT,BANKRUPTCY-NCTUE,NO HIT-LEXISNEXIS   - 21: NO HIT-CREDIT,NO HIT-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 22: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 23: NO HIT-CREDIT,INQ ONLY-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 24: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 25: NO HIT-CREDIT,BKP-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 26: INADEQUATE INFO-CREDIT,BKP-NCTUE,FAIL CA VERIFY-LEXISNEXIS   - 27: NO HIT-CREDIT,NO HIT-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 28: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 29: NO HIT-CREDIT,INQ ONLY-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 30: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 31: NO HIT-CREDIT,BKP-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 32: INADEQUATE INFO-CREDIT,BKP-NCTUE,SECURITY FREEZE-LEXISNEXIS   - 33: NO HIT-CREDIT,NO HIT-NCTUE,SECURITY ALERT-LEXISNEXIS   - 34: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,SECURITY ALERT-LEXISNEXIS   - 35: NO HIT-CREDIT,INQ ONLY-NCTUE,SECURITY ALERT-LEXISNEXIS   - 36: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,SECURITY ALERT-LEXISNEXIS   - 37: NO HIT-CREDIT,BKP-NCTUE,SECURITY ALERT-LEXISNEXIS   - 38: INADEQUATE INFO-CREDIT,BKP-NCTUE,SECURITY ALERT-LEXISNEXIS   - 39: NO HIT-CREDIT,NO HIT-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 40: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 41: NO HIT-CREDIT,INQ ONLY-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 42: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 43: NO HIT-CREDIT,BKP-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 44: INADEQUATE INFO-CREDIT,BKP-NCTUE,DISPUTE ON FILE-LEXISNEXIS   - 45: NO HIT-CREDIT,NO HIT-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 46: INADEQUATE INFO-CREDIT,NO HIT-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 47: NO HIT-CREDIT,INQ ONLY-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 48: INADEQUATE INFO-CREDIT,INQ ONLY-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 49: NO HIT-CREDIT,BKP-NCTUE,INADEQUATE INFO-LEXISNEXIS   - 50: INADEQUATE INFO-CREDIT,BKP-NCTUE,INADEQUATE INFO-LEXISNEXIS   - A1: **NOT AVAILABLE, FILE UNDER REVIEW   - C1: NOT AVAILABLE: NO QUALIFYING ACCOUNTS PRESENT. NO HIT   - C6: XML FORMAT ERROR BETWEEN INTERNAL DATABASES   - C7: INVALID ALTERNATE DATA SOURCE MBR NBR ON DATABASE SERVER   - C8: ALTERNATE DATA SOURCE MBR NBR NOT AUTHORIZED ON DATABASE   - C9: INVALID SECURITY CODE FOR ALTERNATE DATA SRCE ON DATABASE   - CC: NOT AUTHORIZED TO RECEIVE INSIGHT SCORES   - CD: INVALID PRODUCT CONFIGURATION FOR INSIGHT SCORES   - CE: ALTERNATE DATA SOURCE ERROR - MAX OF 4 ADDRESSES ALLOWED   - CF: INVALID ONLINE MATCH CONFIGURATION FOR INSIGHT SCORE   - CG: ALTERNATE DATA SRC ERROR-NO CONSUMER SRCH FIELDS PROVIDED   - CH: MISSING REQUIRED FIELDS FOR INTERNAL DATABASE SEARCH   - CJ: INTERNAL DATABASE ERROR - PLEASE TRY LATER OR CONTACT EFX   - CK: INTERNAL MDS LAYOUT ERROR   - CL: RETRY TRANSACTION TRANSIENT ERROR   - CQ: INTERNAL PROCESSING ERROR PLEASE TRY LATER OR CONTACT EFX   - CS: MODEL SERVER ERROR PLEASE TRY AGAIN LATER OR CONTACT EFX   - CX: HTTP ERROR RETURNED TO OMS FROM NC+/CONNEXUS   - CZ: RETRY TRANSACTION . IF ERROR CONTINUES, CONTACT EQUIFAX   - L0: SUBJECT DECEASED INDICATED   - N1: INQUIRY ONLY FILE   - R0: MODEL DELIVERY IS NOT AVAILABLE: INSUFFICIENT INFORMATION TO SCORE   - X1: MODEL DELIVERY IS TEMPORARILY UNAVAILABLE: PROGRAM NOT DEFINED OR IN THE SYSTEM   - X3: MODEL DELIVERY IS TEMPORARILY UNAVAILABLE: TOO MANY INQUIRIES ON FILE   - X5: MODEL DELIVERY IS TEMPORARILY UNAVAILABLE: MODEL NEEDS INPUT FIELDS AND NONE WERE PROVIDED   - X7: MODEL DELIVERY IS TEMPORARILY UNAVAILABLE: FILE CANNOT BE SCORED  Reject Codes corresponding to Model Number 05184   - C1: MODEL DELIVERY UNAVAILABLE, SCORE RANGE NOT MET   - C2: MODEL DELIVERY UNAVAILABLE, FILE CONDITION NOT MET   - C3: ADDRESS FORMATTED INCORRECTLY FOR ALTERNATE DATA SOURCE   - C4: NAME FORMATTED INCORRECTLY FOR ALTERNATE DATA SOURCE   - C5: BUSINESS NAME INVALID   - C6: XML FORMAT ERROR BETWEEN INTERNAL DATABASES   - C7: INVALID ALTERNATE DATA SOURCE MBR NBR ON DATABASE SERVER   - C8: ALTERNATE DATA SOURCE MBR NBR NOT AUTHORIZED ON DATABASE   - C9: INVALID SECURITY CODE FOR ALTERNATE DATA SRCE ON DATABASE   - CC: NOT AUTHORIZED TO RECEIVE INSIGHT SCORES   - CD: INVALID PRODUCT CONFIGURATION FOR INSIGHT SCORES   - CE: ALTERNATE DATA SOURCE ERROR - MAX OF 4 ADDRESSES ALLOWED   - CF: INVALID ONLINE MATCH CONFIGURATION FOR INSIGHT SCORE   - CG: ALTERNATE DATA SRC ERROR-NO CONSUMER SRCH FIELDS PROVIDED   - CH: MISSING REQUIRED FIELDS FOR INTERNAL DATABASE SEARCH   - CJ: INTERNAL DATABASE ERROR - PLEASE TRY LATER OR CONTACT EFX   - CK: INTERNAL MDB LAYOUT ERROR   - CL: RETRY TRANSACTION TRANSIENT ERROR   - CQ: INTERNAL PROCESSING ERROR PLEASE TRY LATER OR CONTACT EFX   - CS: MODEL SERVER ERROR PLEASE TRY AGAIN LATER OR CONTACT EFX   - CT: INTERNAL CUSTOMER ID ERROR   - CZ: RETRY TRANSACTION - IF ERROR CONTINUES, CONTACT EQUIFAX
    consumerCreditReportRejectsDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportRejects
instance A.FromJSON ConsumerCreditReportRejects where
  parseJSON = A.withObject "ConsumerCreditReportRejects" $ \o ->
    ConsumerCreditReportRejects
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportRejects
instance A.ToJSON ConsumerCreditReportRejects where
  toJSON ConsumerCreditReportRejects {..} =
    _omitNulls
      [ "code" .= consumerCreditReportRejectsCode,
        "description" .= consumerCreditReportRejectsDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportRejects' (by applying it's required fields, if any)
mkConsumerCreditReportRejects ::
  ConsumerCreditReportRejects
mkConsumerCreditReportRejects =
  ConsumerCreditReportRejects
    { consumerCreditReportRejectsCode = Nothing,
      consumerCreditReportRejectsDescription = Nothing
    }

-- ** ConsumerCreditReportEDASRegionalIndicatorCode

-- | ConsumerCreditReportEDASRegionalIndicatorCode
-- Numeric code associated with a geographic region
data ConsumerCreditReportEDASRegionalIndicatorCode = ConsumerCreditReportEDASRegionalIndicatorCode
  { -- | "code" - Code value
    consumerCreditReportEDASRegionalIndicatorCodeCode :: !(Maybe Text),
    -- | "description" -  - 1: SOUTH - 2: WEST - 3: CENTRAL - 4: NORTHEAST
    consumerCreditReportEDASRegionalIndicatorCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportEDASRegionalIndicatorCode
instance A.FromJSON ConsumerCreditReportEDASRegionalIndicatorCode where
  parseJSON = A.withObject "ConsumerCreditReportEDASRegionalIndicatorCode" $ \o ->
    ConsumerCreditReportEDASRegionalIndicatorCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportEDASRegionalIndicatorCode
instance A.ToJSON ConsumerCreditReportEDASRegionalIndicatorCode where
  toJSON ConsumerCreditReportEDASRegionalIndicatorCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportEDASRegionalIndicatorCodeCode,
        "description" .= consumerCreditReportEDASRegionalIndicatorCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportEDASRegionalIndicatorCode' (by applying it's required fields, if any)
mkConsumerCreditReportEDASRegionalIndicatorCode ::
  ConsumerCreditReportEDASRegionalIndicatorCode
mkConsumerCreditReportEDASRegionalIndicatorCode =
  ConsumerCreditReportEDASRegionalIndicatorCode
    { consumerCreditReportEDASRegionalIndicatorCodeCode = Nothing,
      consumerCreditReportEDASRegionalIndicatorCodeDescription = Nothing
    }

-- ** ConsumerCreditReportEDASIndicatorCode

-- | ConsumerCreditReportEDASIndicatorCode
-- The EDAS/FEDAS indicator
data ConsumerCreditReportEDASIndicatorCode = ConsumerCreditReportEDASIndicatorCode
  { -- | "code" - Code value
    consumerCreditReportEDASIndicatorCodeCode :: !(Maybe Text),
    -- | "description" -  - E: Enhanced DAS - F: FACTA EDAS Enhanced Delinquency Alert System (EDAS) and FACTA Enhanced Delinquency Alert System (EDAS) are optional products offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation of the Enhanced Delinquency Alert System (EDAS) option you desire.
    consumerCreditReportEDASIndicatorCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportEDASIndicatorCode
instance A.FromJSON ConsumerCreditReportEDASIndicatorCode where
  parseJSON = A.withObject "ConsumerCreditReportEDASIndicatorCode" $ \o ->
    ConsumerCreditReportEDASIndicatorCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportEDASIndicatorCode
instance A.ToJSON ConsumerCreditReportEDASIndicatorCode where
  toJSON ConsumerCreditReportEDASIndicatorCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportEDASIndicatorCodeCode,
        "description" .= consumerCreditReportEDASIndicatorCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportEDASIndicatorCode' (by applying it's required fields, if any)
mkConsumerCreditReportEDASIndicatorCode ::
  ConsumerCreditReportEDASIndicatorCode
mkConsumerCreditReportEDASIndicatorCode =
  ConsumerCreditReportEDASIndicatorCode
    { consumerCreditReportEDASIndicatorCodeCode = Nothing,
      consumerCreditReportEDASIndicatorCodeDescription = Nothing
    }

-- ** ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode

-- | ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
data ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode = ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
  { -- | "code"
    consumerCreditReportScoreNumberOrMarketMaxIndustryCodeCode :: !(Maybe Text),
    -- | "description" - Either the number of model segments being returned or MarketMax 4.0 industry Code
    consumerCreditReportScoreNumberOrMarketMaxIndustryCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
instance A.FromJSON ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode where
  parseJSON = A.withObject "ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode" $ \o ->
    ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
instance A.ToJSON ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode where
  toJSON ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode {..} =
    _omitNulls
      [ "code" .= consumerCreditReportScoreNumberOrMarketMaxIndustryCodeCode,
        "description" .= consumerCreditReportScoreNumberOrMarketMaxIndustryCodeDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode' (by applying it's required fields, if any)
mkConsumerCreditReportScoreNumberOrMarketMaxIndustryCode ::
  ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
mkConsumerCreditReportScoreNumberOrMarketMaxIndustryCode =
  ConsumerCreditReportScoreNumberOrMarketMaxIndustryCode
    { consumerCreditReportScoreNumberOrMarketMaxIndustryCodeCode = Nothing,
      consumerCreditReportScoreNumberOrMarketMaxIndustryCodeDescription = Nothing
    }

-- ** ConsumerCreditReportAttributes

-- | ConsumerCreditReportAttributes
data ConsumerCreditReportAttributes = ConsumerCreditReportAttributes
  { -- | "identifier" - Variable identifier
    consumerCreditReportAttributesIdentifier :: !(Maybe Text),
    -- | "value" - Value of the variable
    consumerCreditReportAttributesValue :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAttributes
instance A.FromJSON ConsumerCreditReportAttributes where
  parseJSON = A.withObject "ConsumerCreditReportAttributes" $ \o ->
    ConsumerCreditReportAttributes
      <$> (o .:? "identifier")
      <*> (o .:? "value")

-- | ToJSON ConsumerCreditReportAttributes
instance A.ToJSON ConsumerCreditReportAttributes where
  toJSON ConsumerCreditReportAttributes {..} =
    _omitNulls
      [ "identifier" .= consumerCreditReportAttributesIdentifier,
        "value" .= consumerCreditReportAttributesValue
      ]

-- | Construct a value of type 'ConsumerCreditReportAttributes' (by applying it's required fields, if any)
mkConsumerCreditReportAttributes ::
  ConsumerCreditReportAttributes
mkConsumerCreditReportAttributes =
  ConsumerCreditReportAttributes
    { consumerCreditReportAttributesIdentifier = Nothing,
      consumerCreditReportAttributesValue = Nothing
    }

--

-- ** ConsumerCreditReportTypeOfAddress

-- | ConsumerCreditReportTypeOfAddress
-- Type of address
data ConsumerCreditReportTypeOfAddress = ConsumerCreditReportTypeOfAddress
  { -- | "code" - Code value
    consumerCreditReportTypeOfAddressCode :: !(Maybe Text),
    -- | "description" - Description for the given code
    consumerCreditReportTypeOfAddressDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportTypeOfAddress
instance A.FromJSON ConsumerCreditReportTypeOfAddress where
  parseJSON = A.withObject "ConsumerCreditReportTypeOfAddress" $ \o ->
    ConsumerCreditReportTypeOfAddress
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportTypeOfAddress
instance A.ToJSON ConsumerCreditReportTypeOfAddress where
  toJSON ConsumerCreditReportTypeOfAddress {..} =
    _omitNulls
      [ "code" .= consumerCreditReportTypeOfAddressCode,
        "description" .= consumerCreditReportTypeOfAddressDescription
      ]

-- | Construct a value of type 'ConsumerCreditReportTypeOfAddress' (by applying it's required fields, if any)
mkConsumerCreditReportTypeOfAddress ::
  ConsumerCreditReportTypeOfAddress
mkConsumerCreditReportTypeOfAddress =
  ConsumerCreditReportTypeOfAddress
    { consumerCreditReportTypeOfAddressCode = Nothing,
      consumerCreditReportTypeOfAddressDescription = Nothing
    }

-- ** ConsumerCreditReportReturnCode1

-- | ConsumerCreditReportReturnCode1
-- Return code 1
data ConsumerCreditReportReturnCode1 = ConsumerCreditReportReturnCode1
  { -- | "code" - Code value
    consumerCreditReportReturnCode1Code :: !(Maybe Text),
    -- | "description" - Description for the given code
    consumerCreditReportReturnCode1Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportReturnCode1
instance A.FromJSON ConsumerCreditReportReturnCode1 where
  parseJSON = A.withObject "ConsumerCreditReportReturnCode1" $ \o ->
    ConsumerCreditReportReturnCode1
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportReturnCode1
instance A.ToJSON ConsumerCreditReportReturnCode1 where
  toJSON ConsumerCreditReportReturnCode1 {..} =
    _omitNulls
      [ "code" .= consumerCreditReportReturnCode1Code,
        "description" .= consumerCreditReportReturnCode1Description
      ]

-- | Construct a value of type 'ConsumerCreditReportReturnCode1' (by applying it's required fields, if any)
mkConsumerCreditReportReturnCode1 ::
  ConsumerCreditReportReturnCode1
mkConsumerCreditReportReturnCode1 =
  ConsumerCreditReportReturnCode1
    { consumerCreditReportReturnCode1Code = Nothing,
      consumerCreditReportReturnCode1Description = Nothing
    }

-- ** ConsumerCreditReportReturnCode2

-- | ConsumerCreditReportReturnCode2
-- Return code 2
data ConsumerCreditReportReturnCode2 = ConsumerCreditReportReturnCode2
  { -- | "code" - Code value
    consumerCreditReportReturnCode2Code :: !(Maybe Text),
    -- | "description" - Description for the given code
    consumerCreditReportReturnCode2Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportReturnCode2
instance A.FromJSON ConsumerCreditReportReturnCode2 where
  parseJSON = A.withObject "ConsumerCreditReportReturnCode2" $ \o ->
    ConsumerCreditReportReturnCode2
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportReturnCode2
instance A.ToJSON ConsumerCreditReportReturnCode2 where
  toJSON ConsumerCreditReportReturnCode2 {..} =
    _omitNulls
      [ "code" .= consumerCreditReportReturnCode2Code,
        "description" .= consumerCreditReportReturnCode2Description
      ]

-- | Construct a value of type 'ConsumerCreditReportReturnCode2' (by applying it's required fields, if any)
mkConsumerCreditReportReturnCode2 ::
  ConsumerCreditReportReturnCode2
mkConsumerCreditReportReturnCode2 =
  ConsumerCreditReportReturnCode2
    { consumerCreditReportReturnCode2Code = Nothing,
      consumerCreditReportReturnCode2Description = Nothing
    }

-- ** ConsumerCreditReportReturnCode3

-- | ConsumerCreditReportReturnCode3
-- Return code 3
data ConsumerCreditReportReturnCode3 = ConsumerCreditReportReturnCode3
  { -- | "code" - Code value
    consumerCreditReportReturnCode3Code :: !(Maybe Text),
    -- | "description" - Description for the given code
    consumerCreditReportReturnCode3Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportReturnCode3
instance A.FromJSON ConsumerCreditReportReturnCode3 where
  parseJSON = A.withObject "ConsumerCreditReportReturnCode3" $ \o ->
    ConsumerCreditReportReturnCode3
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportReturnCode3
instance A.ToJSON ConsumerCreditReportReturnCode3 where
  toJSON ConsumerCreditReportReturnCode3 {..} =
    _omitNulls
      [ "code" .= consumerCreditReportReturnCode3Code,
        "description" .= consumerCreditReportReturnCode3Description
      ]

-- | Construct a value of type 'ConsumerCreditReportReturnCode3' (by applying it's required fields, if any)
mkConsumerCreditReportReturnCode3 ::
  ConsumerCreditReportReturnCode3
mkConsumerCreditReportReturnCode3 =
  ConsumerCreditReportReturnCode3
    { consumerCreditReportReturnCode3Code = Nothing,
      consumerCreditReportReturnCode3Description = Nothing
    }

-- ** ConsumerCreditReportReturnCode4

-- | ConsumerCreditReportReturnCode4
-- Return code 4
data ConsumerCreditReportReturnCode4 = ConsumerCreditReportReturnCode4
  { -- | "code" - Code value
    consumerCreditReportReturnCode4Code :: !(Maybe Text),
    -- | "description" - Description for the given code
    consumerCreditReportReturnCode4Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportReturnCode4
instance A.FromJSON ConsumerCreditReportReturnCode4 where
  parseJSON = A.withObject "ConsumerCreditReportReturnCode4" $ \o ->
    ConsumerCreditReportReturnCode4
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON ConsumerCreditReportReturnCode4
instance A.ToJSON ConsumerCreditReportReturnCode4 where
  toJSON ConsumerCreditReportReturnCode4 {..} =
    _omitNulls
      [ "code" .= consumerCreditReportReturnCode4Code,
        "description" .= consumerCreditReportReturnCode4Description
      ]

-- | Construct a value of type 'ConsumerCreditReportReturnCode4' (by applying it's required fields, if any)
mkConsumerCreditReportReturnCode4 ::
  ConsumerCreditReportReturnCode4
mkConsumerCreditReportReturnCode4 =
  ConsumerCreditReportReturnCode4
    { consumerCreditReportReturnCode4Code = Nothing,
      consumerCreditReportReturnCode4Description = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage

-- | ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
-- Returned if an error occurs
data ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage = ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
  { -- | "customerReferenceNumber" - This field will return the same value provided in the request
    consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerReferenceNumber :: !(Maybe Text),
    -- | "customerNumber" - This field returns the same value provided in the request
    consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerNumber :: !(Maybe Text),
    -- | "errorType"
    consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageErrorType :: !(Maybe Text),
    -- | "alternateDataSourceCode"
    consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageAlternateDataSourceCode :: !(Maybe [ConsumerCreditReportAlternateDataSourcesAlternateDataSourceCode])
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage" $ \o ->
    ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
      <$> (o .:? "customerReferenceNumber")
      <*> (o .:? "customerNumber")
      <*> (o .:? "errorType")
      <*> (o .:? "alternateDataSourceCode")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage where
  toJSON ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage {..} =
    _omitNulls
      [ "customerReferenceNumber" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerReferenceNumber,
        "customerNumber" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerNumber,
        "errorType" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageErrorType,
        "alternateDataSourceCode" .= consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageAlternateDataSourceCode
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage ::
  ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
mkConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage =
  ConsumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessage
    { consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerReferenceNumber = Nothing,
      consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageCustomerNumber = Nothing,
      consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageErrorType = Nothing,
      consumerCreditReportAlternateDataSourcesAlternateDataSourceErrorMessageAlternateDataSourceCode = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower

-- | ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
-- Provides information relating to the Military Lending Act – MLA Covered Borrower Status.
data ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower = ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
  { -- | "regulatedIdentifier" - RG - indicates that this is Regulated Data
    consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerRegulatedIdentifier :: !(Maybe Text),
    -- | "disclaimer" - Diclaimer Text THE DEPARTMENT OF DEFENSE (\&quot;DOD\&quot;) COVERED BORROWER DATA (\&quot;DATA\&quot;) IS FROM THE DEFENSE MANPOWER DATA CENTER (\&quot;DMDC\&quot;) BY WAY OF CONTRACT BETWEEN EQUIFAX INFORMATION SERVICES LLC (\&quot;EQUIFAX\&quot;) AND DOD. ALL DOD DATA IS USED AND STORED BY EQUIFAX IN ACCORDANCE WITH ITS LEGAL AND CONTRACTUAL OBLIGATIONS. THE DOD DATA IS NOT PART OF EQUIFAX’S NATIONWIDE CREDIT DATABASE, AND EQUIFAX IS REQUIRED TO MAINTAIN THE DOD DATA SEPARATE FROM AND NOT COMMINGLED WITH ANY CREDIT DATA MAINTAINED BY EQUIFAX.
    consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerDisclaimer :: !(Maybe Text),
    -- | "coveredBorrowerStatus" - Yes or No in Military
    consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerCoveredBorrowerStatus :: !(Maybe Text),
    -- | "insufficientDataProvidedForMatch" - Insufficient data provided for match
    consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerInsufficientDataProvidedForMatch :: !(Maybe Text),
    -- | "referralContactNumber" - Contact number for MLA
    consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerReferralContactNumber :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower" $ \o ->
    ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
      <$> (o .:? "regulatedIdentifier")
      <*> (o .:? "disclaimer")
      <*> (o .:? "coveredBorrowerStatus")
      <*> (o .:? "insufficientDataProvidedForMatch")
      <*> (o .:? "referralContactNumber")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower where
  toJSON ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower {..} =
    _omitNulls
      [ "regulatedIdentifier" .= consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerRegulatedIdentifier,
        "disclaimer" .= consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerDisclaimer,
        "coveredBorrowerStatus" .= consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerCoveredBorrowerStatus,
        "insufficientDataProvidedForMatch" .= consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerInsufficientDataProvidedForMatch,
        "referralContactNumber" .= consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerReferralContactNumber
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower ::
  ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
mkConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower =
  ConsumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrower
    { consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerRegulatedIdentifier = Nothing,
      consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerDisclaimer = Nothing,
      consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerCoveredBorrowerStatus = Nothing,
      consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerInsufficientDataProvidedForMatch = Nothing,
      consumerCreditReportAlternateDataSourcesMilitaryLendingCoveredBorrowerReferralContactNumber = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesNorthAmericanLink

-- | ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
-- NAL - Canadian Credit Report for Consumer's Canadian Address
data ConsumerCreditReportAlternateDataSourcesNorthAmericanLink = ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
  { -- | "regulatedIdentifier" - \\&#39;RG\\&#39; for Regulated Data Source
    consumerCreditReportAlternateDataSourcesNorthAmericanLinkRegulatedIdentifier :: !(Maybe Text),
    -- | "hitNohitIndicator" - If there was a responce back from the data source or not
    consumerCreditReportAlternateDataSourcesNorthAmericanLinkHitNohitIndicator :: !(Maybe Text),
    -- | "INTL5FFFConsumerReport" - Contents of the Canadian Report returned
    consumerCreditReportAlternateDataSourcesNorthAmericanLinkIntl5FffConsumerReport :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesNorthAmericanLink where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesNorthAmericanLink" $ \o ->
    ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
      <$> (o .:? "regulatedIdentifier")
      <*> (o .:? "hitNohitIndicator")
      <*> (o .:? "INTL5FFFConsumerReport")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesNorthAmericanLink where
  toJSON ConsumerCreditReportAlternateDataSourcesNorthAmericanLink {..} =
    _omitNulls
      [ "regulatedIdentifier" .= consumerCreditReportAlternateDataSourcesNorthAmericanLinkRegulatedIdentifier,
        "hitNohitIndicator" .= consumerCreditReportAlternateDataSourcesNorthAmericanLinkHitNohitIndicator,
        "INTL5FFFConsumerReport" .= consumerCreditReportAlternateDataSourcesNorthAmericanLinkIntl5FffConsumerReport
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesNorthAmericanLink' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesNorthAmericanLink ::
  ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
mkConsumerCreditReportAlternateDataSourcesNorthAmericanLink =
  ConsumerCreditReportAlternateDataSourcesNorthAmericanLink
    { consumerCreditReportAlternateDataSourcesNorthAmericanLinkRegulatedIdentifier = Nothing,
      consumerCreditReportAlternateDataSourcesNorthAmericanLinkHitNohitIndicator = Nothing,
      consumerCreditReportAlternateDataSourcesNorthAmericanLinkIntl5FffConsumerReport = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts

-- | ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
-- FraudIQ Synthetic ID Alerts provide real‐time alerts that help determine if the identity presented is real or synthetic
data ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts = ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
  { -- | "nonRegulatedIdentifier" - \\&#39;NR\\&#39; Non Regulated Data Source
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNonRegulatedIdentifier :: !(Maybe Text),
    -- | "hitNohitIndicator" - If there was a response back from the data source or not
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsHitNohitIndicator :: !(Maybe Text),
    -- | "disclaimer" - DISCLAIMER:[SYNTHETIC ID ALERT]  Indicator is for identity fraud risk alert purposes only and is not to be used for determining an individual’s eligibility for any credit or any other FCRA permissible purpose. Client shall use the [SYNTHETIC ID ALERT]  exclusively within Client’s own organization for the purpose of identity fraud prevention.  Accordingly, Client will not use an alert or warning message from the [SYNTHETIC ID ALERT] system in its decision-making process for denying credit, but will use the message as an indication that the consumer&#39;s identity and personally identifiable information should be independently verified to form a reasonable belief that it knows the true identity of the consumer. Client understands that the information supplied by [SYNTHETIC ID ALERT] may or may not apply to the consumer who has applied to Client for credit, service, dealings, or other financial transactions. Client also understands and agrees that data from the [SYNTHETIC ID ALERT] system is proprietary to Equifax and shall not be used as a component of any database or file built or maintained by Client. The use of such data shall be limited to the specific transaction for which the [SYNTHETIC ID ALERT] message is provided. Equifax may, by written notice to Client, immediately terminate the Client’s agreement for service or suspend the [SYNTHETIC ID ALERT] service if Equifax has a reasonable belief that Client has violated the terms of this disclaimer or the agreement for service.
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsDisclaimer :: !(Maybe Text),
    -- | "finalAssessmentFlag" - Final Synthetic ID Alert assessment will be Y if either Authorized User Velocity (AUV) Flag or ID Discrepancy Flag are Y
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsFinalAssessmentFlag :: !(Maybe Text),
    -- | "authorizedUserVelocityFlag" - Flag will be Y if combination of Number Authorized users &amp; Number Terminated trades meets the defined criteria for the AUV Flag
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsAuthorizedUserVelocityFlag :: !(Maybe Text),
    -- | "idDiscrepancyFlag" - Flag will be Y if it meets the defined criteria for the current Identity discrepancy attributes (based on patterns exhibited by synthetic fraudsters and identity mismatches between authorized users and primary account holders like count of last name mismatches, etc)
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsIdDiscrepancyFlag :: !(Maybe Text),
    -- | "numberOfAuthorizedUsers" - Count of active Authorized Users potentially associated with consumer
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfAuthorizedUsers :: !(Maybe Text),
    -- | "numberOfTerminatedUsers" - Count of Terminated Trades potentially associated with consumer
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfTerminatedUsers :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts" $ \o ->
    ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
      <$> (o .:? "nonRegulatedIdentifier")
      <*> (o .:? "hitNohitIndicator")
      <*> (o .:? "disclaimer")
      <*> (o .:? "finalAssessmentFlag")
      <*> (o .:? "authorizedUserVelocityFlag")
      <*> (o .:? "idDiscrepancyFlag")
      <*> (o .:? "numberOfAuthorizedUsers")
      <*> (o .:? "numberOfTerminatedUsers")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts where
  toJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts {..} =
    _omitNulls
      [ "nonRegulatedIdentifier" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNonRegulatedIdentifier,
        "hitNohitIndicator" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsHitNohitIndicator,
        "disclaimer" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsDisclaimer,
        "finalAssessmentFlag" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsFinalAssessmentFlag,
        "authorizedUserVelocityFlag" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsAuthorizedUserVelocityFlag,
        "idDiscrepancyFlag" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsIdDiscrepancyFlag,
        "numberOfAuthorizedUsers" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfAuthorizedUsers,
        "numberOfTerminatedUsers" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfTerminatedUsers
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts ::
  ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
mkConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts =
  ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlerts
    { consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNonRegulatedIdentifier = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsHitNohitIndicator = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsDisclaimer = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsFinalAssessmentFlag = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsAuthorizedUserVelocityFlag = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsIdDiscrepancyFlag = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfAuthorizedUsers = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDAlertsNumberOfTerminatedUsers = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts

-- | ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
-- FraudIQ Synthetic ID Ver 2 Alerts (Expanded) provide real‐time alerts that help determine if the identity presented is real or synthetic. The syntheticIDVer2 object will be returned in JSON only and includes a Disclaimer, Alert Flags and a Billing Code.
data ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts = ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
  { -- | "nonRegulatedIdentifier" - \\&#39;NR\\&#39; Non Regulated Data Source
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsNonRegulatedIdentifier :: !(Maybe Text),
    -- | "hitNohitIndicator" - If there was a response back from the data source or not
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsHitNohitIndicator :: !(Maybe Text),
    -- | "syntheticIdVer2" - FraudIQ Synthetic ID Ver 2 Alerts (Expanded) provide real‐time alerts that help determine if the identity presented is real or synthetic.
    consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsSyntheticIdVer2 :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts" $ \o ->
    ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
      <$> (o .:? "nonRegulatedIdentifier")
      <*> (o .:? "hitNohitIndicator")
      <*> (o .:? "syntheticIdVer2")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts where
  toJSON ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts {..} =
    _omitNulls
      [ "nonRegulatedIdentifier" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsNonRegulatedIdentifier,
        "hitNohitIndicator" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsHitNohitIndicator,
        "syntheticIdVer2" .= consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsSyntheticIdVer2
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts ::
  ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
mkConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts =
  ConsumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2Alerts
    { consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsNonRegulatedIdentifier = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsHitNohitIndicator = Nothing,
      consumerCreditReportAlternateDataSourcesFraudIQSyntheticIDV2AlertsSyntheticIdVer2 = Nothing
    }

-- ** ConsumerCreditReportAlternateDataSourcesErrorCodes

-- | ConsumerCreditReportAlternateDataSourcesErrorCodes
-- Describes the error
data ConsumerCreditReportAlternateDataSourcesErrorCodes = ConsumerCreditReportAlternateDataSourcesErrorCodes
  { -- | "verbiage" - Verbiage returned for error code
    consumerCreditReportAlternateDataSourcesErrorCodesVerbiage :: !(Maybe Text),
    -- | "errorText" - Error description
    consumerCreditReportAlternateDataSourcesErrorCodesErrorText :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON ConsumerCreditReportAlternateDataSourcesErrorCodes
instance A.FromJSON ConsumerCreditReportAlternateDataSourcesErrorCodes where
  parseJSON = A.withObject "ConsumerCreditReportAlternateDataSourcesErrorCodes" $ \o ->
    ConsumerCreditReportAlternateDataSourcesErrorCodes
      <$> (o .:? "verbiage")
      <*> (o .:? "errorText")

-- | ToJSON ConsumerCreditReportAlternateDataSourcesErrorCodes
instance A.ToJSON ConsumerCreditReportAlternateDataSourcesErrorCodes where
  toJSON ConsumerCreditReportAlternateDataSourcesErrorCodes {..} =
    _omitNulls
      [ "verbiage" .= consumerCreditReportAlternateDataSourcesErrorCodesVerbiage,
        "errorText" .= consumerCreditReportAlternateDataSourcesErrorCodesErrorText
      ]

-- | Construct a value of type 'ConsumerCreditReportAlternateDataSourcesErrorCodes' (by applying it's required fields, if any)
mkConsumerCreditReportAlternateDataSourcesErrorCodes ::
  ConsumerCreditReportAlternateDataSourcesErrorCodes
mkConsumerCreditReportAlternateDataSourcesErrorCodes =
  ConsumerCreditReportAlternateDataSourcesErrorCodes
    { consumerCreditReportAlternateDataSourcesErrorCodesVerbiage = Nothing,
      consumerCreditReportAlternateDataSourcesErrorCodesErrorText = Nothing
    }
