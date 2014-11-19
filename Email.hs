module Email
       ( Email
       , ToAddress(..)
       , FromAddress(..)
       , EmailBody(..)
       , RecipientName(..)
       ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson (FromJSON, eitherDecode, Value(..), (.:), parseJSON)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
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

mkEmail :: ToAddress
        -> FromAddress
        -> EmailBody
        -> RecipientName
        -> Validation [EmailErrors] Email
mkEmail to@(ToAddress toTxt) from@(FromAddress fromTxt)
        body name = Email <$> toV <*> fromV <*> pure body <*> pure name
  where toB   = validateEmail toTxt
        fromB = validateEmail fromTxt
        toV   = bool (Failure [ToAddressDidntParse]) (Success to) toB
        fromV = bool (Failure [FromAddressDidntParse]) (Success from) fromB

data EmailErrors = ToAddressDidntParse
                 | FromAddressDidntParse deriving (Eq, Show)

goodJson :: BL.ByteString
goodJson = pack $
           unlines ["{\"to\":   \"levi@startup.com\",",
                    "\"from\": \"chris@website.org\",",
                    "\"body\": \"hello!\",",
                    "\"name\": \"Levi\"}"]

badJson :: BL.ByteString
badJson = pack $
          unlines ["{\"to\":   \"levi@startup.com\",",
                   "\"from\": \"chris@website.org\",",
                   "\"body\": \"hello!\"}"]

instance FromJSON ToAddress where
  parseJSON (String v) = bool mzero (pure (ToAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON FromAddress where
  parseJSON (String v) = bool mzero (pure (FromAddress v)) (validateEmail v)
  parseJSON _ = mzero

instance FromJSON EmailBody where
  parseJSON (String v) = pure (EmailBody v)
  parseJSON _ = mzero

instance FromJSON RecipientName where
  parseJSON (String v) = pure (RecipientName v)
  parseJSON _ = mzero


instance FromJSON Email where
  parseJSON (Object v) = Email       <$>
                         v .: "to"   <*>
                         v .: "from" <*>
                         v .: "body" <*>
                         v .: "name"
  parseJSON _          = mzero

main :: IO ()
main = putStrLn "hello"
