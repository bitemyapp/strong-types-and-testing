module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (join, mzero)
import Data.Aeson (FromJSON, eitherDecode, Value(..), (.:), parseJSON)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (either)
import Data.Either.Validation
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)


newtype ToAddress     = ToAddress     T.Text deriving (Eq, Show)
newtype FromAddress   = FromAddress   T.Text deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data Email = Email {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)

validateEmail :: T.Text -> Bool
validateEmail = isValid . encodeUtf8

type EmailValidation a = Validation [EmailErrors] a

mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> EmailValidation Email
mkEmail to@(ToAddress toTxt) from@(FromAddress fromTxt)
        body name = Email <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = validateEmail toTxt
        fromB = validateEmail fromTxt
        toV   = bool (Failure [ToAddressDidntParse]) (Success to) toB
        fromV = bool (Failure [FromAddressDidntParse]) (Success from) fromB

mkEmailV :: EmailValidation ToAddress
        -> EmailValidation FromAddress
        -> EmailValidation EmailBody
        -> EmailValidation RecipientName
        -> EmailValidation Email
mkEmailV to from body name = Email <$> to <*> from <*> body <*> name

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse
                 | BadJsonForEmail String
                 | BadJsonToAddress
                 | BadJsonFromAddress deriving (Eq, Show)

goodJson :: BL.ByteString
goodJson = pack $
           unlines ["{\"to\":   \"levi@startup.com\",",
                    "\"from\": \"chris@website.org\",",
                    "\"body\": \"hello!\",",
                    "\"name\": \"Levi\"}"]

jsonMissingKey :: BL.ByteString
jsonMissingKey = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chris@website.org\",",
                   "\"body\": \"hello!\"}"]

jsonBadEmail :: BL.ByteString
jsonBadEmail = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chrisLOL\",",
                   "\"body\": \"hello!\",",
                   "\"name\": \"Levi\"}"]

instance FromJSON ToAddress where
  parseJSON (String v) = bool (fail "ToAddress failed validation")
                         (pure (ToAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON FromAddress where
  parseJSON (String v) = bool (fail "FromAddress failed validation")
                         (pure (FromAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON EmailBody where
  parseJSON (String v) = pure (EmailBody v)
  parseJSON _ = mzero

instance FromJSON RecipientName where
  parseJSON (String v) = pure (RecipientName v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation ToAddress) where
  parseJSON (String v) = bool (pure . Failure $ [BadJsonToAddress])
                         ((pure . pure) $ (ToAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation FromAddress) where
  parseJSON (String v) = bool (pure . Failure $ [BadJsonFromAddress])
                         ((pure . pure) $ (FromAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation EmailBody) where
  parseJSON (String v) = (pure . pure) $ (EmailBody v)
  parseJSON _ = mzero

instance FromJSON (EmailValidation RecipientName) where
  parseJSON (String v) = (pure . pure) $ (RecipientName v)
  parseJSON _ = mzero


instance FromJSON (EmailValidation Email) where
  parseJSON (Object v) =    mkEmailV  <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero

parseEmailJSON :: BL.ByteString -> EmailValidation Email
parseEmailJSON = either (Failure . return . BadJsonForEmail) id . eitherDecode

main :: IO ()
main = do
  let printJSON = print . parseEmailJSON
  printJSON goodJson
  printJSON jsonMissingKey
  printJSON jsonBadEmail
