{-# LANGUAGE OverloadedStrings #-}

module TheEquifax.Core.Auth where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Data as P (Typeable, typeOf)
import qualified Data.Foldable as P
import Data.Function ((&))
import qualified Data.Text.Encoding as T
import qualified Lens.Micro as L
import TheEquifax.Core
import Prelude (Applicative, Bool (..), Char, Double, FilePath, Float, Functor, Int, Integer, Maybe (..), Monad, String, fmap, maybe, mempty, pure, undefined, ($), (.), (/=), (<$>), (<*>), (=<<), (>>=))
import qualified Prelude as P

-- * Auth Methods

-- ** AuthBasicOAuth20

data AuthBasicOAuth20
  = -- | username password
    AuthBasicOAuth20 B.ByteString B.ByteString
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthBasicOAuth20 where
  applyAuthMethod _ a@(AuthBasicOAuth20 user pw) req =
    P.pure $
      if (P.typeOf a `P.elem` rAuthTypes req)
        then
          req `setHeader` toHeader ("Authorization", T.decodeUtf8 cred)
            & L.over rAuthTypesL (P.filter (/= P.typeOf a))
        else req
    where
      cred = BC.append "Basic " (B64.encode $ BC.concat [user, ":", pw])

-- ** OAuth20 Token from "/v2/oauth/token"

newtype AuthEquaifaxOAuth20Token
  = -- | token
    AuthEquaifaxOAuth20Token B.ByteString
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthEquaifaxOAuth20Token where
  applyAuthMethod _ a@(AuthEquaifaxOAuth20Token token) req =
    P.pure $
      if (P.typeOf a `P.elem` rAuthTypes req)
        then
          req `setHeader` toHeader ("Authorization", T.decodeUtf8 cred)
            & L.over rAuthTypesL (P.filter (/= P.typeOf a))
        else req
    where
      cred = BC.append "Bearer " token
