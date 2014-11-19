module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (join, mzero)
import Data.Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (either)
import qualified Data.Either.Validation as V
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Email.Validate (isValid)


newtype Email         = Email         T.Text deriving (Eq, Show)
newtype ToAddress     = ToAddress     Email  deriving (Eq, Show)
newtype FromAddress   = FromAddress   Email  deriving (Eq, Show)
newtype EmailBody     = EmailBody     T.Text deriving (Eq, Show)
newtype RecipientName = RecipientName T.Text deriving (Eq, Show)

data EmailForm = EmailForm {
    toAddress     :: ToAddress
  , fromAddress   :: FromAddress
  , emailBody     :: EmailBody
  , recipientName :: RecipientName
} deriving (Eq, Show)

validateEmail :: T.Text -> Bool
validateEmail = isValid . encodeUtf8

validEmail :: T.Text -> Maybe Email
validEmail v = if validateEmail v then
                 Just (Email v)
                 else
                 Nothing

type EmailValidation a = V.Validation [EmailErrors] a

mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> EmailValidation EmailForm
mkEmail to@(ToAddress (Email toTxt)) from@(FromAddress (Email fromTxt))
        body name = EmailForm <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = validateEmail toTxt
        fromB = validateEmail fromTxt
        toV   = bool (V.Failure [ToAddressDidntParse]) (V.Success to) toB
        fromV = bool (V.Failure [FromAddressDidntParse]) (V.Success from) fromB

mkEmailV :: EmailValidation ToAddress
        -> EmailValidation FromAddress
        -> EmailValidation EmailBody
        -> EmailValidation RecipientName
        -> EmailValidation EmailForm
mkEmailV to from body name = EmailForm <$> to <*> from <*> body <*> name

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse
                 | BadJsonForEmail String
                 | BadJsonToAddress
                 | BadJsonFromAddress
                 | BadJsonEmailBody
                 | BadJsonRecipientName deriving (Eq, Show)

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

instance FromJSON Email where
  parseJSON (String v) = bool (fail "Email failed validation")
                         (pure (Email v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON ToAddress where
  parseJSON (String v) = case validEmail v of
    (Just _) -> pure (ToAddress (Email v))
    Nothing  -> fail "ToAddress email failed validation"
  parseJSON _ = mzero

instance FromJSON FromAddress where
  parseJSON (String v) = case validEmail v of
    (Just _) -> pure (FromAddress (Email v))
    Nothing  -> fail "FromAddress email failed validation"
    -- bool (fail "FromAddress failed validation")
    -- (pure (FromAddress (Email v))) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON EmailBody where
  parseJSON (String v) = pure (EmailBody v)
  parseJSON _ = mzero

instance FromJSON RecipientName where
  parseJSON (String v) = pure (RecipientName v)
  parseJSON _ = mzero

getJSON :: (FromJSON a, Applicative f) =>
           e -> (a -> b) -> Value -> f (V.Validation [e] b)
getJSON e f v = case from of
  (Error _) -> pure $ V.Failure [e]
  (Success a) -> pure $ V.Success (f a)
  where from = fromJSON v

instance FromJSON (EmailValidation ToAddress) where
  parseJSON v = getJSON BadJsonFromAddress ToAddress v

instance FromJSON (EmailValidation FromAddress) where
  parseJSON v = case address of
    (Error _)   -> pure $ V.Failure [BadJsonFromAddress]
    (Success a) -> pure $ V.Success (FromAddress a)
    where address = fromJSON v

instance FromJSON (EmailValidation EmailBody) where
  parseJSON v = case body of
    (Error _)   -> pure $ V.Failure [BadJsonEmailBody]
    (Success a) -> pure $ V.Success a
    where body = fromJSON v

instance FromJSON (EmailValidation RecipientName) where
  parseJSON v = case name of
    (Error _)   -> pure $ V.Failure [BadJsonRecipientName]
    (Success a) -> pure $ V.Success a
    where name = fromJSON v


instance FromJSON (EmailValidation EmailForm) where
  parseJSON (Object v) =    mkEmailV  <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero

parseEmailJSON :: BL.ByteString -> EmailValidation EmailForm
parseEmailJSON = either (V.Failure . return . BadJsonForEmail) id . eitherDecode

main :: IO ()
main = do
  let printJSON = print . parseEmailJSON
  printJSON goodJson
  printJSON jsonMissingKey
  printJSON jsonBadEmail
