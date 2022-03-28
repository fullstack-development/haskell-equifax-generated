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

module TheEquifax.Models.Trade where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Data as P (Typeable)
import Data.Text (Text)
import TheEquifax.Core
import TheEquifax.Models.CreditorClassificationCode
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (=<<), (>>=))
import qualified Prelude as P

-- Abstract and unify type of Trade from ConsumerCreditReport API and PrescreenOfOne API

-- ** Trade

-- | Trade
data Trade = Trade
  { -- | "automatedUpdateIndicator" - Automated Update Indicator
    tradeAutomatedUpdateIndicator :: !(Maybe Text),
    -- | "monthsReviewed" - How many months the trade was reviewed
    tradeMonthsReviewed :: !(Maybe Text),
    -- | "accountDesignator"
    tradeAccountDesignator :: !(Maybe AccountDesignatorCode),
    -- | "accountNumber" - Account number
    tradeAccountNumber :: !(Maybe Text),
    -- | "thirtyDayCounter" - Number of times the consumer has been 30 days delinquent in payment
    tradeThirtyDayCounter :: !(Maybe Int),
    -- | "sixtyDayCounter" - Number of times the consumer has been 60 days delinquent in payment
    tradeSixtyDayCounter :: !(Maybe Int),
    -- | "ninetyDayCounter" - Number of times the consumer has been 90+ days delinquent in payment
    tradeNinetyDayCounter :: !(Maybe Int),
    -- | "previousHighRate1" - First highest rate that occurred outside of the timeframe of the payment history that has beed requested (Valid Rates\\: 2-5, 8 and 9)
    tradePreviousHighRate1 :: !(Maybe Int),
    -- | "previousHighDate1" - Date in format MMYYYY where MM is the Month and YYYY is the year
    tradePreviousHighDate1 :: !(Maybe Text),
    -- | "previousHighRate2" - Second highest rate that occurred outside of the timeframe of the payment history that has beed requested (Valid Rates\\: 2-5, 8 and 9)
    tradePreviousHighRate2 :: !(Maybe Int),
    -- | "previousHighDate2" - Date in format MMYYYY where MM is the Month and YYYY is the year
    tradePreviousHighDate2 :: !(Maybe Text),
    -- | "previousHighRate3" - Third highest rate that occurred outside of the timeframe of the payment history that has beed requested (Valid Rates\\: 2-5, 8 and 9)
    tradePreviousHighRate3 :: !(Maybe Int),
    -- | "previousHighDate3" - Date in format MMYYYY where MM is the Month and YYYY is the year
    tradePreviousHighDate3 :: !(Maybe Text),
    -- | "24MonthPaymentHistory"
    trade24monthPaymentHistory :: !(Maybe [Trade24MonthPaymentHistory]),
    -- | "customerName" - Customer name
    tradeCustomerName :: !(Maybe Text),
    -- | "customerNumber" - Customer number
    tradeCustomerNumber :: !(Maybe Text),
    -- | "dateReported" - Represents the \&quot;as of\&quot; date of the most recent update received from the data furnisher. Accounts may continue to be updated by the data furnisher even after they are closed, paid out or become severely delinquent (charge off, repossession, etc.).
    tradeDateReported :: !(Maybe Date),
    -- | "dateOpened" - Date account originally opened
    tradeDateOpened :: !(Maybe Date),
    -- | "highCredit" - Highest outstanding balance or original amount
    tradeHighCredit :: !(Maybe Int),
    -- | "creditLimit" - Highest available credit limit
    tradeCreditLimit :: !(Maybe Int),
    -- | "balance" - Account balance
    tradeBalance :: !(Maybe Int),
    -- | "pastDueAmount" - Amount past due on an account (late)
    tradePastDueAmount :: !(Maybe Int),
    -- | "portfolioTypeCode"
    tradePortfolioTypeCode :: !(Maybe TradePortfolioTypeCode),
    -- | "rateStatusCode"
    tradeRateStatusCode :: !(Maybe TradeRateStatusCode),
    -- | "rate"
    tradeRate :: !(Maybe TradeRate),
    -- | "lastActivityDate" - Contains the date of last activity
    tradeLastActivityDate :: !(Maybe Text),
    -- | "narrativeCodes"
    tradeNarrativeCodes :: !(Maybe [A.Value]),
    -- | "rawNarrativeCodes" - Raw codes for the narratives
    tradeRawNarrativeCodes :: !(Maybe [Text]),
    -- | "accountTypeCode"
    tradeAccountTypeCode :: !(Maybe AccountTypeCode),
    -- | "lastPaymentDate" - Date of the most recent payment.
    tradeLastPaymentDate :: !(Maybe Date),
    -- | "closedDate" - Contains the date the account was closed
    tradeClosedDate :: !(Maybe Date),
    -- | "dateMajorDelinquencyFirstReported" - If current Rate/Status code is 6, 7, 8, 9, M, Z or if trade contains narrative code 081 (foreclosure), this date will reflect the first time that Rate/Status or narrative code was reported.
    tradeDateMajorDelinquencyFirstReported :: !(Maybe Date),
    -- | "actualPaymentAmount" - Payment actually received for a reporting period
    tradeActualPaymentAmount :: !(Maybe Int),
    -- | "scheduledPaymentAmount" - Contractual amount due for a payment period. (The figure in this field should be the monthly amount due regardless of the actual payment frequency.)
    tradeScheduledPaymentAmount :: !(Maybe Int),
    -- | "termsFrequencyCode"
    tradeTermsFrequencyCode :: !(Maybe TradeTermsFrequencyCode),
    -- | "termsDurationCode"
    tradeTermsDurationCode :: !(Maybe TradeTermsDurationCode),
    -- | "purchasedFromOrSoldCreditorIndicator"
    tradePurchasedFromOrSoldCreditorIndicator :: !(Maybe TradePurchasedFromOrSoldCreditorIndicator),
    -- | "purchasedFromOrSoldCreditorName" - Denotes who the account was either purchased from or sold to or the Original Creditor’s name
    tradePurchasedFromOrSoldCreditorName :: !(Maybe Text),
    -- | "creditorClassificationCode"
    tradeCreditorClassificationCode :: !(Maybe CreditorClassificationCode),
    -- | "activityDesignatorCode"
    tradeActivityDesignatorCode :: !(Maybe TradeActivityDesignatorCode),
    -- | "originalChargeOffAmount" - Amount originally charged to loss by the creditor
    tradeOriginalChargeOffAmount :: !(Maybe Int),
    -- | "deferredPaymentStartDate" - Contains the date the first payment is due for deferred loans
    tradeDeferredPaymentStartDate :: !(Maybe Date),
    -- | "ballonPaymentAmount" - Amount of the balloon payment
    tradeBallonPaymentAmount :: !(Maybe Int),
    -- | "ballonPaymentDueDate" - Contains the date the balloon payment is due
    tradeBallonPaymentDueDate :: !(Maybe Date),
    -- | "mortgageIDNumber" - Number assigned to a mortgage loan that remains constant throughout the life of the loan. The MIN indicates that the loan is registered with the Mortgage Electronic Registration Systems, Inc., the electronic registry for tracking the ownership of mortgage rights.
    tradeMortgageIdNumber :: !(Maybe Text),
    -- | "paymentHistory1to24"
    tradePaymentHistory1to24 :: !(Maybe [TradePaymentHistory1to24]),
    -- | "paymentHistory25to36"
    tradePaymentHistory25to36 :: !(Maybe [TradePaymentHistory25to36]),
    -- | "paymentHistory37to48"
    tradePaymentHistory37to48 :: !(Maybe [TradePaymentHistory37to48]),
    -- | "previousHighRatePaymentHistory"
    tradePreviousHighRatePaymentHistory :: !(Maybe Text),
    -- | "previousHighDatePaymentHistory" - Date in format MMYYYY where MM is the Month and YYYY is the year
    tradePreviousHighDatePaymentHistory :: !(Maybe Text),
    -- | "dimensionsDataStartDate" - Date in format MMYYYY where MM is the Month and YYYY is the year
    tradeDimensionsDataStartDate :: !(Maybe Text),
    -- | "dimensionsNumberOfMonths" - Number of months of Dimensions data requested
    tradeDimensionsNumberOfMonths :: !(Maybe Text),
    -- | "dimension" - Information over the most recent 24 months of historical data. There may be gaps of historical Dimensions data if that month (or field) is not available Dimensions Data is an optional feature offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation.
    tradeDimension :: !(Maybe [TradeDimension])
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Trade
instance A.FromJSON Trade where
  parseJSON = A.withObject "Trade" $ \o ->
    Trade
      <$> (o .:? "automatedUpdateIndicator")
      <*> (o .:? "monthsReviewed")
      <*> (o .:? "accountDesignator")
      <*> (o .:? "accountNumber")
      <*> (o .:? "thirtyDayCounter")
      <*> (o .:? "sixtyDayCounter")
      <*> (o .:? "ninetyDayCounter")
      <*> (o .:? "previousHighRate1")
      <*> (o .:? "previousHighDate1")
      <*> (o .:? "previousHighRate2")
      <*> (o .:? "previousHighDate2")
      <*> (o .:? "previousHighRate3")
      <*> (o .:? "previousHighDate3")
      <*> (o .:? "24MonthPaymentHistory")
      <*> (o .:? "customerName")
      <*> (o .:? "customerNumber")
      <*> (o .:? "dateReported")
      <*> (o .:? "dateOpened")
      <*> (o .:? "highCredit")
      <*> (o .:? "creditLimit")
      <*> (o .:? "balance")
      <*> (o .:? "pastDueAmount")
      <*> (o .:? "portfolioTypeCode")
      <*> (o .:? "rateStatusCode")
      <*> (o .:? "rate")
      <*> (o .:? "lastActivityDate")
      <*> (o .:? "narrativeCodes")
      <*> (o .:? "rawNarrativeCodes")
      <*> (o .:? "accountTypeCode")
      <*> (o .:? "lastPaymentDate")
      <*> (o .:? "closedDate")
      <*> (o .:? "dateMajorDelinquencyFirstReported")
      <*> (o .:? "actualPaymentAmount")
      <*> (o .:? "scheduledPaymentAmount")
      <*> (o .:? "termsFrequencyCode")
      <*> (o .:? "termsDurationCode")
      <*> (o .:? "purchasedFromOrSoldCreditorIndicator")
      <*> (o .:? "purchasedFromOrSoldCreditorName")
      <*> (o .:? "creditorClassificationCode")
      <*> (o .:? "activityDesignatorCode")
      <*> (o .:? "originalChargeOffAmount")
      <*> (o .:? "deferredPaymentStartDate")
      <*> (o .:? "ballonPaymentAmount")
      <*> (o .:? "ballonPaymentDueDate")
      <*> (o .:? "mortgageIDNumber")
      <*> (o .:? "paymentHistory1to24")
      <*> (o .:? "paymentHistory25to36")
      <*> (o .:? "paymentHistory37to48")
      <*> (o .:? "previousHighRatePaymentHistory")
      <*> (o .:? "previousHighDatePaymentHistory")
      <*> (o .:? "dimensionsDataStartDate")
      <*> (o .:? "dimensionsNumberOfMonths")
      <*> (o .:? "dimension")

-- | ToJSON Trade
instance A.ToJSON Trade where
  toJSON Trade {..} =
    _omitNulls
      [ "automatedUpdateIndicator" .= tradeAutomatedUpdateIndicator,
        "monthsReviewed" .= tradeMonthsReviewed,
        "accountDesignator" .= tradeAccountDesignator,
        "accountNumber" .= tradeAccountNumber,
        "thirtyDayCounter" .= tradeThirtyDayCounter,
        "sixtyDayCounter" .= tradeSixtyDayCounter,
        "ninetyDayCounter" .= tradeNinetyDayCounter,
        "previousHighRate1" .= tradePreviousHighRate1,
        "previousHighDate1" .= tradePreviousHighDate1,
        "previousHighRate2" .= tradePreviousHighRate2,
        "previousHighDate2" .= tradePreviousHighDate2,
        "previousHighRate3" .= tradePreviousHighRate3,
        "previousHighDate3" .= tradePreviousHighDate3,
        "24MonthPaymentHistory" .= trade24monthPaymentHistory,
        "customerName" .= tradeCustomerName,
        "customerNumber" .= tradeCustomerNumber,
        "dateReported" .= tradeDateReported,
        "dateOpened" .= tradeDateOpened,
        "highCredit" .= tradeHighCredit,
        "creditLimit" .= tradeCreditLimit,
        "balance" .= tradeBalance,
        "pastDueAmount" .= tradePastDueAmount,
        "portfolioTypeCode" .= tradePortfolioTypeCode,
        "rateStatusCode" .= tradeRateStatusCode,
        "rate" .= tradeRate,
        "lastActivityDate" .= tradeLastActivityDate,
        "narrativeCodes" .= tradeNarrativeCodes,
        "rawNarrativeCodes" .= tradeRawNarrativeCodes,
        "accountTypeCode" .= tradeAccountTypeCode,
        "lastPaymentDate" .= tradeLastPaymentDate,
        "closedDate" .= tradeClosedDate,
        "dateMajorDelinquencyFirstReported" .= tradeDateMajorDelinquencyFirstReported,
        "actualPaymentAmount" .= tradeActualPaymentAmount,
        "scheduledPaymentAmount" .= tradeScheduledPaymentAmount,
        "termsFrequencyCode" .= tradeTermsFrequencyCode,
        "termsDurationCode" .= tradeTermsDurationCode,
        "purchasedFromOrSoldCreditorIndicator" .= tradePurchasedFromOrSoldCreditorIndicator,
        "purchasedFromOrSoldCreditorName" .= tradePurchasedFromOrSoldCreditorName,
        "creditorClassificationCode" .= tradeCreditorClassificationCode,
        "activityDesignatorCode" .= tradeActivityDesignatorCode,
        "originalChargeOffAmount" .= tradeOriginalChargeOffAmount,
        "deferredPaymentStartDate" .= tradeDeferredPaymentStartDate,
        "ballonPaymentAmount" .= tradeBallonPaymentAmount,
        "ballonPaymentDueDate" .= tradeBallonPaymentDueDate,
        "mortgageIDNumber" .= tradeMortgageIdNumber,
        "paymentHistory1to24" .= tradePaymentHistory1to24,
        "paymentHistory25to36" .= tradePaymentHistory25to36,
        "paymentHistory37to48" .= tradePaymentHistory37to48,
        "previousHighRatePaymentHistory" .= tradePreviousHighRatePaymentHistory,
        "previousHighDatePaymentHistory" .= tradePreviousHighDatePaymentHistory,
        "dimensionsDataStartDate" .= tradeDimensionsDataStartDate,
        "dimensionsNumberOfMonths" .= tradeDimensionsNumberOfMonths,
        "dimension" .= tradeDimension
      ]

-- | Construct a value of type 'Trade' (by applying it's required fields, if any)
mkTrade ::
  Trade
mkTrade =
  Trade
    { tradeAutomatedUpdateIndicator = Nothing,
      tradeMonthsReviewed = Nothing,
      tradeAccountDesignator = Nothing,
      tradeAccountNumber = Nothing,
      tradeThirtyDayCounter = Nothing,
      tradeSixtyDayCounter = Nothing,
      tradeNinetyDayCounter = Nothing,
      tradePreviousHighRate1 = Nothing,
      tradePreviousHighDate1 = Nothing,
      tradePreviousHighRate2 = Nothing,
      tradePreviousHighDate2 = Nothing,
      tradePreviousHighRate3 = Nothing,
      tradePreviousHighDate3 = Nothing,
      trade24monthPaymentHistory = Nothing,
      tradeCustomerName = Nothing,
      tradeCustomerNumber = Nothing,
      tradeDateReported = Nothing,
      tradeDateOpened = Nothing,
      tradeHighCredit = Nothing,
      tradeCreditLimit = Nothing,
      tradeBalance = Nothing,
      tradePastDueAmount = Nothing,
      tradePortfolioTypeCode = Nothing,
      tradeRateStatusCode = Nothing,
      tradeRate = Nothing,
      tradeLastActivityDate = Nothing,
      tradeNarrativeCodes = Nothing,
      tradeRawNarrativeCodes = Nothing,
      tradeAccountTypeCode = Nothing,
      tradeLastPaymentDate = Nothing,
      tradeClosedDate = Nothing,
      tradeDateMajorDelinquencyFirstReported = Nothing,
      tradeActualPaymentAmount = Nothing,
      tradeScheduledPaymentAmount = Nothing,
      tradeTermsFrequencyCode = Nothing,
      tradeTermsDurationCode = Nothing,
      tradePurchasedFromOrSoldCreditorIndicator = Nothing,
      tradePurchasedFromOrSoldCreditorName = Nothing,
      tradeCreditorClassificationCode = Nothing,
      tradeActivityDesignatorCode = Nothing,
      tradeOriginalChargeOffAmount = Nothing,
      tradeDeferredPaymentStartDate = Nothing,
      tradeBallonPaymentAmount = Nothing,
      tradeBallonPaymentDueDate = Nothing,
      tradeMortgageIdNumber = Nothing,
      tradePaymentHistory1to24 = Nothing,
      tradePaymentHistory25to36 = Nothing,
      tradePaymentHistory37to48 = Nothing,
      tradePreviousHighRatePaymentHistory = Nothing,
      tradePreviousHighDatePaymentHistory = Nothing,
      tradeDimensionsDataStartDate = Nothing,
      tradeDimensionsNumberOfMonths = Nothing,
      tradeDimension = Nothing
    }

-- ** AccountDesignatorCode

-- | AccountDesignatorCode
-- Consumer relationship with account
data AccountDesignatorCode = AccountDesignatorCode
  { -- | "code" - Code value
    accountDesignatorCodeCode :: !(Maybe Text),
    -- | "description" - - A: Authorized User – This is an authorized user of this account; another individual has contractual responsibility. - B: On behalf of another person – The subject has financial responsibility for an account, which is used exclusively by another person. - C: Co-maker – The subject has co-signed for a loan, and will be responsible for payment if the borrower should default. - I: Individual Account – The subject of the report has contractual responsibility for this account and is primarily responsible for its payment. - J: Joint Account – The subject and another person (or persons) are jointly responsible for payment on this account. - M: Maker – The subject is responsible for payment of a loan, but a co-maker will be responsible for payment if maker defaults. - S: Shared, but otherwise undesignated – This code is an indication that the credit grantor knows that the subject and at least one other person share the account, but not enough information is available to designate the account as “J” or “A”. - T: Terminated – The subject’s relationship to this account has ended, although other parties who once shared the account may continue to maintain the account. - U: Undesignated - X: Deceased (Not returned on Trade Lines)
    accountDesignatorCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AccountDesignatorCode
instance A.FromJSON AccountDesignatorCode where
  parseJSON = A.withObject "AccountDesignatorCode" $ \o ->
    AccountDesignatorCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON AccountDesignatorCode
instance A.ToJSON AccountDesignatorCode where
  toJSON AccountDesignatorCode {..} =
    _omitNulls
      [ "code" .= accountDesignatorCodeCode,
        "description" .= accountDesignatorCodeDescription
      ]

-- | Construct a value of type 'AccountDesignatorCode' (by applying it's required fields, if any)
mkAccountDesignatorCode ::
  AccountDesignatorCode
mkAccountDesignatorCode =
  AccountDesignatorCode
    { accountDesignatorCodeCode = Nothing,
      accountDesignatorCodeDescription = Nothing
    }

-- ** Trade24MonthPaymentHistory

-- | Trade24MonthPaymentHistory
data Trade24MonthPaymentHistory = Trade24MonthPaymentHistory
  { -- | "code"
    trade24MonthPaymentHistoryCode :: !(Maybe Text),
    -- | "description" - The most recent payment history on the trade for the last 24 months It&#39;s an optional feature offered by Equifax. Please contact your Equifax Sales Associate for additional information and activation.
    trade24MonthPaymentHistoryDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON Trade24MonthPaymentHistory
instance A.FromJSON Trade24MonthPaymentHistory where
  parseJSON = A.withObject "Trade24MonthPaymentHistory" $ \o ->
    Trade24MonthPaymentHistory
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON Trade24MonthPaymentHistory
instance A.ToJSON Trade24MonthPaymentHistory where
  toJSON Trade24MonthPaymentHistory {..} =
    _omitNulls
      [ "code" .= trade24MonthPaymentHistoryCode,
        "description" .= trade24MonthPaymentHistoryDescription
      ]

-- | Construct a value of type 'Trade24MonthPaymentHistory' (by applying it's required fields, if any)
mkTrade24MonthPaymentHistory ::
  Trade24MonthPaymentHistory
mkTrade24MonthPaymentHistory =
  Trade24MonthPaymentHistory
    { trade24MonthPaymentHistoryCode = Nothing,
      trade24MonthPaymentHistoryDescription = Nothing
    }

-- ** TradePortfolioTypeCode

-- | TradePortfolioTypeCode
-- Describes the payment arrangement of the account (revolving, open, installment, line of credit or mortgage)
data TradePortfolioTypeCode = TradePortfolioTypeCode
  { -- | "code" - Code value
    tradePortfolioTypeCodeCode :: !(Maybe Text),
    -- | "description" - Portfolio Type Codes:   - C: Line of Credit (payment amounts based on the outstanding balance)   - I: Installment (fixed number of payments)   - M: Mortgage (fixed number of payments – usually for real estate)   - O: Open Account (entire balance is due upon demand)   - R: Revolving (payment amounts based on the outstanding balance)   - Blank: No Portfolio Type available
    tradePortfolioTypeCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradePortfolioTypeCode
instance A.FromJSON TradePortfolioTypeCode where
  parseJSON = A.withObject "TradePortfolioTypeCode" $ \o ->
    TradePortfolioTypeCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradePortfolioTypeCode
instance A.ToJSON TradePortfolioTypeCode where
  toJSON TradePortfolioTypeCode {..} =
    _omitNulls
      [ "code" .= tradePortfolioTypeCodeCode,
        "description" .= tradePortfolioTypeCodeDescription
      ]

-- | Construct a value of type 'TradePortfolioTypeCode' (by applying it's required fields, if any)
mkTradePortfolioTypeCode ::
  TradePortfolioTypeCode
mkTradePortfolioTypeCode =
  TradePortfolioTypeCode
    { tradePortfolioTypeCodeCode = Nothing,
      tradePortfolioTypeCodeDescription = Nothing
    }

-- ** TradeRateStatusCode

-- | TradeRateStatusCode
-- Current status or rating of the account
data TradeRateStatusCode = TradeRateStatusCode
  { -- | "code" - Code value
    tradeRateStatusCodeCode :: !(Maybe Text),
    -- | "description" -  - 0: Too new to rate; Approved but not used    : - 1: Pays account as agreed    : - 2: Not more than two payments past due    : - 3: Not more than three payments past due    : - 4: Not more than four payments past due    : - 5: At least 120 days or more than four payments past due    : - 6: Collection account (Enhanced Trade Only)    : - 7: Included in Chapter 13    : - 8: Repossession    : - 9: Charge-off - Blank: No rate reported
    tradeRateStatusCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradeRateStatusCode
instance A.FromJSON TradeRateStatusCode where
  parseJSON = A.withObject "TradeRateStatusCode" $ \o ->
    TradeRateStatusCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradeRateStatusCode
instance A.ToJSON TradeRateStatusCode where
  toJSON TradeRateStatusCode {..} =
    _omitNulls
      [ "code" .= tradeRateStatusCodeCode,
        "description" .= tradeRateStatusCodeDescription
      ]

-- | Construct a value of type 'TradeRateStatusCode' (by applying it's required fields, if any)
mkTradeRateStatusCode ::
  TradeRateStatusCode
mkTradeRateStatusCode =
  TradeRateStatusCode
    { tradeRateStatusCodeCode = Nothing,
      tradeRateStatusCodeDescription = Nothing
    }

-- ** TradeRate

-- | TradeRate
data TradeRate = TradeRate
  { -- | "code"
    tradeRateCode :: !(Maybe Int),
    -- | "description"
    tradeRateDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradeRate
instance A.FromJSON TradeRate where
  parseJSON = A.withObject "TradeRate" $ \o ->
    TradeRate
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradeRate
instance A.ToJSON TradeRate where
  toJSON TradeRate {..} =
    _omitNulls
      [ "code" .= tradeRateCode,
        "description" .= tradeRateDescription
      ]

-- | Construct a value of type 'TradeRate' (by applying it's required fields, if any)
mkTradeRate ::
  TradeRate
mkTradeRate =
  TradeRate
    { tradeRateCode = Nothing,
      tradeRateDescription = Nothing
    }

-- ** AccountTypeCode

-- | AccountTypeCode
-- Codes describing the account type
data AccountTypeCode = AccountTypeCode
  { -- | "code" - Code value
    accountTypeCodeCode :: !(Maybe Text),
    -- | "description" - Account Type Codes:   - 00: Auto   - 01: Unsecured   - 02: Secured   - 03: Partially Secured   - 04: Home Improvement   - 05: Federal Housing Administration Home Improvement   - 06: Installment Sales Contract   - 07: Charge Account   - 08: Real Estate   - 10: Business Loan - Individual is personally liable   - 11: Recreational Merchandise   - 12: Education Loan   - 13: Lease   - 14: Co-Maker Not Borrower (Retired 9-18-09)   - 15: Line of Credit   - 17: Manufactured Housing   - 18: Credit Card   - 19: Federal Housing Administration Real Estate Mortgage   - 20: Note Loan   - 21: Note Loan With Co-Maker   - 22: Secured By Household Goods   - 23: Secured By Household Goods/Collateral   - 25: Veteran’s Administration Real Estate Mortgage   - 26: Conventional Real Estate Mortgage   - 27: Real Estate Mortgage   - 29: Rental Agreement   - 34: Debt Counseling Service   - 37: Combined Credit Plan (represents two credit plans being reported as one account)   - 43: Debit Card (used when backed by a line of credit or overdraft protection)   - 47: Credit Line Secured   - 48: Collection Agency/Attorney   - 49: Insurance Claim Pending   - 50: Family Support   - 65: Government Unsecured Guaranteed Loan   - 66: Government Secured Guaranteed Loan   - 67: Government Unsecured Direct Loan   - 68: Government Secured Direct Loan   - 69: Government Grant   - 70: Government Overpayment   - 71: Government Fine   - 72: Government Fee for Services   - 73: Government Employee Advance   - 74: Government Miscellaneous Debt   - 75: Government Benefit   - 77: Returned Check   - 78: Installment Loan   - 85: Bi-Monthly Mortgage Payment (every other month)   - 87: Semi-Monthly Mortgage Payment (twice per month)   - 89: Home Equity Line of Credit   - 90: Medical Debt   - 91: Debt Consolidation   - 92: Utility Company   - 93: Child Support   - 94: Spouse Support   - 95: Attorney Fees   - 0A: Time Share Loan (a purchased time share)   - 1A: Lender Placed Insurance   - 2A: Secured Credit Card   - 3A: Auto Lease   - 5A: Real Estate (junior liens and non-purchase money first)   - 6A: Commercial Installment Loan (Individual personally liable; company is guarantor)   - 7A: Commercial Line of Credit (Individual personally liable; company is guarantor)   - 8A: Business Credit Card (Individual has primary responsibility)   - 9A: Secured Home Improvement   - 5B: Second Mortgage   - 6B: Commercial Mortgage Loan (Individual is personally liable; company is guarantor)   - 7B: Agricultural   - 8B: Deposit Related (overdrawn account)   - 9B: Business Line Personally Guaranteed   - 0C: Debt Buyer Account   - 1C: Household Goods   - 2C: US Department of Agriculture Real Estate Mortgage Loan   - 4D: Telecommunication/Cellular   - 6D: Home Equity   - 0F: Construction Loan   - 0G: Flexible Spending Credit Card
    accountTypeCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON AccountTypeCode
instance A.FromJSON AccountTypeCode where
  parseJSON = A.withObject "AccountTypeCode" $ \o ->
    AccountTypeCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON AccountTypeCode
instance A.ToJSON AccountTypeCode where
  toJSON AccountTypeCode {..} =
    _omitNulls
      [ "code" .= accountTypeCodeCode,
        "description" .= accountTypeCodeDescription
      ]

-- | Construct a value of type 'AccountTypeCode' (by applying it's required fields, if any)
mkAccountTypeCode ::
  AccountTypeCode
mkAccountTypeCode =
  AccountTypeCode
    { accountTypeCodeCode = Nothing,
      accountTypeCodeDescription = Nothing
    }

--

-- ** TradeTermsFrequencyCode

-- | TradeTermsFrequencyCode
-- Reflects how often payments are due
data TradeTermsFrequencyCode = TradeTermsFrequencyCode
  { -- | "code" - Code value
    tradeTermsFrequencyCodeCode :: !(Maybe Text),
    -- | "description" - - B: Biweekly (due every 2 weeks) - D: Deferred - E: Semi-monthly (due twice a month) - L: Bi-monthly (due every 2 months) - M: Monthly (due every month) - P: Single Payment Loan - Q: Quarterly (due every 3 months) - S: Semi-annually (due twice a year) - T: Tri-annually (due every 4 months) - W: Weekly (due every week) - Y: Annually (due every year)
    tradeTermsFrequencyCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradeTermsFrequencyCode
instance A.FromJSON TradeTermsFrequencyCode where
  parseJSON = A.withObject "TradeTermsFrequencyCode" $ \o ->
    TradeTermsFrequencyCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradeTermsFrequencyCode
instance A.ToJSON TradeTermsFrequencyCode where
  toJSON TradeTermsFrequencyCode {..} =
    _omitNulls
      [ "code" .= tradeTermsFrequencyCodeCode,
        "description" .= tradeTermsFrequencyCodeDescription
      ]

-- | Construct a value of type 'TradeTermsFrequencyCode' (by applying it's required fields, if any)
mkTradeTermsFrequencyCode ::
  TradeTermsFrequencyCode
mkTradeTermsFrequencyCode =
  TradeTermsFrequencyCode
    { tradeTermsFrequencyCodeCode = Nothing,
      tradeTermsFrequencyCodeDescription = Nothing
    }

-- ** TradeTermsDurationCode

-- | TradeTermsDurationCode
-- Amount of time to repay the loan
data TradeTermsDurationCode = TradeTermsDurationCode
  { -- | "code" - Code value
    tradeTermsDurationCodeCode :: !(Maybe Text),
    -- | "description" - Terms duration codes    - D: Days   - M: Months   - Y: Years
    tradeTermsDurationCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradeTermsDurationCode
instance A.FromJSON TradeTermsDurationCode where
  parseJSON = A.withObject "TradeTermsDurationCode" $ \o ->
    TradeTermsDurationCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradeTermsDurationCode
instance A.ToJSON TradeTermsDurationCode where
  toJSON TradeTermsDurationCode {..} =
    _omitNulls
      [ "code" .= tradeTermsDurationCodeCode,
        "description" .= tradeTermsDurationCodeDescription
      ]

-- | Construct a value of type 'TradeTermsDurationCode' (by applying it's required fields, if any)
mkTradeTermsDurationCode ::
  TradeTermsDurationCode
mkTradeTermsDurationCode =
  TradeTermsDurationCode
    { tradeTermsDurationCodeCode = Nothing,
      tradeTermsDurationCodeDescription = Nothing
    }

-- ** TradePurchasedFromOrSoldCreditorIndicator

-- | TradePurchasedFromOrSoldCreditorIndicator
-- Purchased From/Sold To/Original Creditor Name is the purchaser, seller or original creditor
data TradePurchasedFromOrSoldCreditorIndicator = TradePurchasedFromOrSoldCreditorIndicator
  { -- | "code" - Code value
    tradePurchasedFromOrSoldCreditorIndicatorCode :: !(Maybe Text),
    -- | "description" - Purchased From/Sold To/Original Creditor Values   - O: Original Creditor   - P: Purchased From   - S: Sold To
    tradePurchasedFromOrSoldCreditorIndicatorDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradePurchasedFromOrSoldCreditorIndicator
instance A.FromJSON TradePurchasedFromOrSoldCreditorIndicator where
  parseJSON = A.withObject "TradePurchasedFromOrSoldCreditorIndicator" $ \o ->
    TradePurchasedFromOrSoldCreditorIndicator
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradePurchasedFromOrSoldCreditorIndicator
instance A.ToJSON TradePurchasedFromOrSoldCreditorIndicator where
  toJSON TradePurchasedFromOrSoldCreditorIndicator {..} =
    _omitNulls
      [ "code" .= tradePurchasedFromOrSoldCreditorIndicatorCode,
        "description" .= tradePurchasedFromOrSoldCreditorIndicatorDescription
      ]

-- | Construct a value of type 'TradePurchasedFromOrSoldCreditorIndicator' (by applying it's required fields, if any)
mkTradePurchasedFromOrSoldCreditorIndicator ::
  TradePurchasedFromOrSoldCreditorIndicator
mkTradePurchasedFromOrSoldCreditorIndicator =
  TradePurchasedFromOrSoldCreditorIndicator
    { tradePurchasedFromOrSoldCreditorIndicatorCode = Nothing,
      tradePurchasedFromOrSoldCreditorIndicatorDescription = Nothing
    }

-- ** TradeActivityDesignatorCode

-- | TradeActivityDesignatorCode
-- Describes the final state of the account.
data TradeActivityDesignatorCode = TradeActivityDesignatorCode
  { -- | "code" - Code value
    tradeActivityDesignatorCodeCode :: !(Maybe Text),
    -- | "description" - - B: Paid and Closed - C: Closed - D: Transfer/Sold/Paid - L: Lost/Stolen - P: Paid - R: Refinanced - T: Transfer/Sold
    tradeActivityDesignatorCodeDescription :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradeActivityDesignatorCode
instance A.FromJSON TradeActivityDesignatorCode where
  parseJSON = A.withObject "TradeActivityDesignatorCode" $ \o ->
    TradeActivityDesignatorCode
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradeActivityDesignatorCode
instance A.ToJSON TradeActivityDesignatorCode where
  toJSON TradeActivityDesignatorCode {..} =
    _omitNulls
      [ "code" .= tradeActivityDesignatorCodeCode,
        "description" .= tradeActivityDesignatorCodeDescription
      ]

-- | Construct a value of type 'TradeActivityDesignatorCode' (by applying it's required fields, if any)
mkTradeActivityDesignatorCode ::
  TradeActivityDesignatorCode
mkTradeActivityDesignatorCode =
  TradeActivityDesignatorCode
    { tradeActivityDesignatorCodeCode = Nothing,
      tradeActivityDesignatorCodeDescription = Nothing
    }

-- ** TradeDimension

-- | TradeDimension
data TradeDimension = TradeDimension
  { -- | "dimensionsBalance" - Balance during the month being reported
    tradeDimensionDimensionsBalance :: !(Maybe Int),
    -- | "dimensionsActualPaymentAmount" - Payment actually received for a reporting period
    tradeDimensionDimensionsActualPaymentAmount :: !(Maybe Int),
    -- | "dimensionsScheduledPaymentAmount" - Contractual amount due for a payment period. (The figure in this field should be the monthly amount due regardless of the actual payment frequency.)
    tradeDimensionDimensionsScheduledPaymentAmount :: !(Maybe Int),
    -- | "dimensionsLastPaymentDate" - Date of the most recent payment
    tradeDimensionDimensionsLastPaymentDate :: !(Maybe Date),
    -- | "dimensionsHighCredit" - Highest outstanding balance or original amount
    tradeDimensionDimensionsHighCredit :: !(Maybe Int),
    -- | "dimensionsCreditLimit" - Highest available credit amount
    tradeDimensionDimensionsCreditLimit :: !(Maybe Int),
    -- | "dimensionsPastDueAmount" - Current amount overdue on a trade
    tradeDimensionDimensionsPastDueAmount :: !(Maybe Int),
    -- | "dimensionsNarrativeCodes"
    tradeDimensionDimensionsNarrativeCodes :: !(Maybe [A.Value]),
    -- | "dimensionsRawNarrativeCodes" - Raw Dimensions Data naratives. It contains up to 4 narratives
    tradeDimensionDimensionsRawNarrativeCodes :: !(Maybe [Text]),
    -- | "dimensionsAccountDesignatorCode"
    tradeDimensionDimensionsAccountDesignatorCode :: !(Maybe AccountDesignatorCode),
    -- | "dimensionsAccountTypeCode"
    tradeDimensionDimensionsAccountTypeCode :: !(Maybe AccountTypeCode)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradeDimension
instance A.FromJSON TradeDimension where
  parseJSON = A.withObject "TradeDimension" $ \o ->
    TradeDimension
      <$> (o .:? "dimensionsBalance")
      <*> (o .:? "dimensionsActualPaymentAmount")
      <*> (o .:? "dimensionsScheduledPaymentAmount")
      <*> (o .:? "dimensionsLastPaymentDate")
      <*> (o .:? "dimensionsHighCredit")
      <*> (o .:? "dimensionsCreditLimit")
      <*> (o .:? "dimensionsPastDueAmount")
      <*> (o .:? "dimensionsNarrativeCodes")
      <*> (o .:? "dimensionsRawNarrativeCodes")
      <*> (o .:? "dimensionsAccountDesignatorCode")
      <*> (o .:? "dimensionsAccountTypeCode")

-- | ToJSON TradeDimension
instance A.ToJSON TradeDimension where
  toJSON TradeDimension {..} =
    _omitNulls
      [ "dimensionsBalance" .= tradeDimensionDimensionsBalance,
        "dimensionsActualPaymentAmount" .= tradeDimensionDimensionsActualPaymentAmount,
        "dimensionsScheduledPaymentAmount" .= tradeDimensionDimensionsScheduledPaymentAmount,
        "dimensionsLastPaymentDate" .= tradeDimensionDimensionsLastPaymentDate,
        "dimensionsHighCredit" .= tradeDimensionDimensionsHighCredit,
        "dimensionsCreditLimit" .= tradeDimensionDimensionsCreditLimit,
        "dimensionsPastDueAmount" .= tradeDimensionDimensionsPastDueAmount,
        "dimensionsNarrativeCodes" .= tradeDimensionDimensionsNarrativeCodes,
        "dimensionsRawNarrativeCodes" .= tradeDimensionDimensionsRawNarrativeCodes,
        "dimensionsAccountDesignatorCode" .= tradeDimensionDimensionsAccountDesignatorCode,
        "dimensionsAccountTypeCode" .= tradeDimensionDimensionsAccountTypeCode
      ]

-- | Construct a value of type 'TradeDimension' (by applying it's required fields, if any)
mkTradeDimension ::
  TradeDimension
mkTradeDimension =
  TradeDimension
    { tradeDimensionDimensionsBalance = Nothing,
      tradeDimensionDimensionsActualPaymentAmount = Nothing,
      tradeDimensionDimensionsScheduledPaymentAmount = Nothing,
      tradeDimensionDimensionsLastPaymentDate = Nothing,
      tradeDimensionDimensionsHighCredit = Nothing,
      tradeDimensionDimensionsCreditLimit = Nothing,
      tradeDimensionDimensionsPastDueAmount = Nothing,
      tradeDimensionDimensionsNarrativeCodes = Nothing,
      tradeDimensionDimensionsRawNarrativeCodes = Nothing,
      tradeDimensionDimensionsAccountDesignatorCode = Nothing,
      tradeDimensionDimensionsAccountTypeCode = Nothing
    }

-- ** TradePaymentHistory1to24

-- | TradePaymentHistory1to24
data TradePaymentHistory1to24 = TradePaymentHistory1to24
  { -- | "code"
    tradePaymentHistory1to24Code :: !(Maybe Text),
    -- | "description" - The valid values for the standard 24, 36 and 48 Month Payment History are:    - Blank: not populated based on the Date Open (payment history will only be populated for each of the months that the account has been open)   - E: Zero balance and current account    - \\*: Rate/Status was not available for that month  - 2 – 6, 8 and 9 (See Rate/Status Codes for values)  - 6: Valid value for Payment History
    tradePaymentHistory1to24Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradePaymentHistory1to24
instance A.FromJSON TradePaymentHistory1to24 where
  parseJSON = A.withObject "TradePaymentHistory1to24" $ \o ->
    TradePaymentHistory1to24
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradePaymentHistory1to24
instance A.ToJSON TradePaymentHistory1to24 where
  toJSON TradePaymentHistory1to24 {..} =
    _omitNulls
      [ "code" .= tradePaymentHistory1to24Code,
        "description" .= tradePaymentHistory1to24Description
      ]

-- | Construct a value of type 'TradePaymentHistory1to24' (by applying it's required fields, if any)
mkTradePaymentHistory1to24 ::
  TradePaymentHistory1to24
mkTradePaymentHistory1to24 =
  TradePaymentHistory1to24
    { tradePaymentHistory1to24Code = Nothing,
      tradePaymentHistory1to24Description = Nothing
    }

-- ** TradePaymentHistory25to36

-- | TradePaymentHistory25to36
data TradePaymentHistory25to36 = TradePaymentHistory25to36
  { -- | "code"
    tradePaymentHistory25to36Code :: !(Maybe Text),
    -- | "description" - The valid values for the standard 24, 36 and 48 Month Payment History are:  - Blank: not populated based on the Date Open (payment history will only be populated for each of the months that the account has been open)  - E: Zero balance and current account  - \\*: Rate/Status was not available for that month  - 2 - 6, 8 and 9 (See Rate/Status Codes for values)  - 6: Valid value for Payment History
    tradePaymentHistory25to36Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradePaymentHistory25to36
instance A.FromJSON TradePaymentHistory25to36 where
  parseJSON = A.withObject "TradePaymentHistory25to36" $ \o ->
    TradePaymentHistory25to36
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradePaymentHistory25to36
instance A.ToJSON TradePaymentHistory25to36 where
  toJSON TradePaymentHistory25to36 {..} =
    _omitNulls
      [ "code" .= tradePaymentHistory25to36Code,
        "description" .= tradePaymentHistory25to36Description
      ]

-- | Construct a value of type 'TradePaymentHistory25to36' (by applying it's required fields, if any)
mkTradePaymentHistory25to36 ::
  TradePaymentHistory25to36
mkTradePaymentHistory25to36 =
  TradePaymentHistory25to36
    { tradePaymentHistory25to36Code = Nothing,
      tradePaymentHistory25to36Description = Nothing
    }

-- ** TradePaymentHistory37to48

-- | TradePaymentHistory37to48
data TradePaymentHistory37to48 = TradePaymentHistory37to48
  { -- | "code"
    tradePaymentHistory37to48Code :: !(Maybe Text),
    -- | "description" - The valid values for the standard 24, 36 and 48 Month Payment History are:  - Blank: not populated based on the Date Open (payment history will only be populated for each of the months that the account has been open)  - E: Zero balance and current account  - \\*: Rate/Status was not available for that month  - 2 – 6, 8 and 9 (See Rate/Status Codes for values)  - 6: Valid value for Payment History
    tradePaymentHistory37to48Description :: !(Maybe Text)
  }
  deriving (P.Show, P.Eq, P.Typeable)

-- | FromJSON TradePaymentHistory37to48
instance A.FromJSON TradePaymentHistory37to48 where
  parseJSON = A.withObject "TradePaymentHistory37to48" $ \o ->
    TradePaymentHistory37to48
      <$> (o .:? "code")
      <*> (o .:? "description")

-- | ToJSON TradePaymentHistory37to48
instance A.ToJSON TradePaymentHistory37to48 where
  toJSON TradePaymentHistory37to48 {..} =
    _omitNulls
      [ "code" .= tradePaymentHistory37to48Code,
        "description" .= tradePaymentHistory37to48Description
      ]

-- | Construct a value of type 'TradePaymentHistory37to48' (by applying it's required fields, if any)
mkTradePaymentHistory37to48 ::
  TradePaymentHistory37to48
mkTradePaymentHistory37to48 =
  TradePaymentHistory37to48
    { tradePaymentHistory37to48Code = Nothing,
      tradePaymentHistory37to48Description = Nothing
    }
