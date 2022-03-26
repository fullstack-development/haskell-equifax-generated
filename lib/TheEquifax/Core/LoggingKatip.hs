{-
   Prescreen of One

   # Introduction      Equifax’s Prescreen of One is a sales tool that is intended to entice the consumer to take a step toward opening a credit or loan account. Lenders can make offers of credit or insurance, in real time, in person or over the phone.    # Getting Started  1. **<a href=\"/user/applications\" target=\"_blank\">Create</a>** an application  2. **<a href=\"/user/applications\" target=\"_blank\">Subscribe</a>** to Prescreen of One API  3. **<a href=\"/products/consumer-credit-report\" target=\"_blank\">Explore</a>** the Sandbox environment  # Promoting to UAT  To successfully submit test transactions through your application in the UAT environment, a test member number is required.  You may use your existing Equifax test member number(s).  Please contact your Equifax Account Representative if a new test member number is needed. The steps below can be used to test in the UAT environment.  Transactions generated using a test member number do not return identical results as a sandbox transaction.  A sandbox transaction will reflect everything that is exposed in the API whereas a test member number is configured to align with the specifications of your contract. Please contact us to ensure your test member number is configured to your expectations.      1. **<a href=\"/user/applications\" target=\"_blank\">Promote</a>** application to test  2. **<a href=\"/documentation\" target=\"_blank\">Update</a>** requests details for UAT  3. **<a href=\"/documentation\" target=\"_blank\">Test</a>** scenarios in UAT environment   # For More Details    * **<a href=\"/contact\" target=\"_blank\">Contact Us</a>** - Equifax Developer Center Support Website   * **ACRO Migration Support**       * phone: 1-888-407-0359; Option 2 followed by Option 5         * email:  BT.Acro.tech@equifax.com        

   OpenAPI Version: 3.0.0
   Prescreen of One API version: 1.0.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : PrescreenOfOne.LoggingKatip
Katip Logging functions
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TheEquifax.Core.LoggingKatip where

import qualified Control.Exception.Safe as E
import qualified Control.Monad.IO.Class as P
import qualified Control.Monad.Trans.Reader as P
import qualified Data.Text as T
import qualified Lens.Micro as L
import qualified System.IO as IO

import Data.Text (Text)
import GHC.Exts (IsString(..))

import qualified Katip as LG

-- * Type Aliases (for compatibility)

-- | Runs a Katip logging block with the Log environment
type LogExecWithContext = forall m a. P.MonadIO m =>
                                      LogContext -> LogExec m a

-- | A Katip logging block
type LogExec m a = LG.KatipT m a -> m a

-- | A Katip Log environment
type LogContext = LG.LogEnv

-- | A Katip Log severity
type LogLevel = LG.Severity

-- * default logger

-- | the default log environment
initLogContext :: IO LogContext
initLogContext = LG.initLogEnv "PrescreenOfOne" "dev"

-- | Runs a Katip logging block with the Log environment
runDefaultLogExecWithContext :: LogExecWithContext
runDefaultLogExecWithContext = LG.runKatipT

-- * stdout logger

-- | Runs a Katip logging block with the Log environment
stdoutLoggingExec :: LogExecWithContext
stdoutLoggingExec = runDefaultLogExecWithContext

-- | A Katip Log environment which targets stdout
stdoutLoggingContext :: LogContext -> IO LogContext
stdoutLoggingContext cxt = do
    handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stdout (LG.permitItem LG.InfoS) LG.V2
    LG.registerScribe "stdout" handleScribe LG.defaultScribeSettings cxt

-- * stderr logger

-- | Runs a Katip logging block with the Log environment
stderrLoggingExec :: LogExecWithContext
stderrLoggingExec = runDefaultLogExecWithContext

-- | A Katip Log environment which targets stderr
stderrLoggingContext :: LogContext -> IO LogContext
stderrLoggingContext cxt = do
    handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stderr (LG.permitItem LG.InfoS) LG.V2
    LG.registerScribe "stderr" handleScribe LG.defaultScribeSettings cxt

-- * Null logger

-- | Disables Katip logging
runNullLogExec :: LogExecWithContext
runNullLogExec le (LG.KatipT f) = P.runReaderT f (L.set LG.logEnvScribes mempty le)

-- * Log Msg

-- | Log a katip message
_log :: (Applicative m, LG.Katip m) => Text -> LogLevel -> Text -> m ()
_log src level msg = do
  LG.logMsg (fromString $ T.unpack src) level (LG.logStr msg)

-- * Log Exceptions

-- | re-throws exceptions after logging them
logExceptions
  :: (LG.Katip m, E.MonadCatch m, Applicative m)
  => Text -> m a -> m a
logExceptions src =
  E.handle
    (\(e :: E.SomeException) -> do
       _log src LG.ErrorS ((T.pack . show) e)
       E.throw e)

-- * Log Level

levelInfo :: LogLevel
levelInfo = LG.InfoS

levelError :: LogLevel
levelError = LG.ErrorS

levelDebug :: LogLevel
levelDebug = LG.DebugS