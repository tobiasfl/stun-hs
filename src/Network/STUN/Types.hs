module Network.STUN.Types
    (Message
    , mkMessage
    , msgType
    , transactionId
    , attributes
    , MessageType(..)
    , TransactionId(..)
    , Address(..)
    , Attribute(..)
    , ErrCode(..)
    , mkMappedAddress
    , mkXORMappedAddress
    , mkErrorCode)
    where

import Network.Socket (PortNumber, HostAddress, HostAddress6)
import qualified Data.Text as T
import qualified Data.ByteString as BS
--import qualified Data.Text.Encoding as TE
--import Data.Text.Encoding.Error (lenientDecode)
--import Data.Either (either)
--import Data.Bifunctor (first)

newtype TransactionId = TransactionId BS.ByteString
    deriving (Eq, Show)

data MessageType =
    BindingRequest
  | BindingSuccessResponse
  | BindingErrorResponse
  deriving (Eq, Show, Enum, Bounded)

data Address = IPv4 HostAddress | IPv6 HostAddress6
    deriving (Eq, Show)

--newtype OpaqueString = OpaqueString { unOpaqueString :: T.Text }
--  deriving (Eq, Show)
--
--toWireFormat :: OpaqueString -> BS.StrictByteString
--toWireFormat = TE.encodeUtf8 . unOpaqueString
--
--mkOpaqueString :: BS.ByteString -> Int -> Either String OpaqueString
--mkOpaqueString bs maxLength
--    | T.null text = Left "OpaqueString cannot be empty"
--    | BS.length bs > maxLength = Left "OpaqueString too long"
--    | otherwise = Right (OpaqueString text)
--    where text = TE.decodeUtf8With lenientDecode bs

data ErrCode =
    TryAlternate300
  | BadRequest400
  | Unauthenticated401
  | UnknownAttribute420
  | StaleNonce438
  | ServerError500
  deriving (Eq, Show, Enum, Bounded)

data Attribute =
    MappedAddress PortNumber Address
  | XORMappedAddress PortNumber Address
--  | UserName OpaqueString
--  | UserHash
--  | MessageIntegrity
--  | MessageIntegritySHA256
--  | FingerPrint
  | ErrorCode ErrCode T.Text
--  | Realm
--  | Nonce
--  | PasswordAlgorithms
--  | PasswordAlgorithm
--  | UnknownAttributes
--  | Software
--  | AlternateServer
--  | AlternateDomain
    deriving (Eq, Show)

mkMappedAddress :: PortNumber -> Address -> Attribute
mkMappedAddress = MappedAddress

mkXORMappedAddress :: PortNumber -> Address -> Attribute
mkXORMappedAddress = XORMappedAddress

mkErrorCode :: ErrCode -> T.Text -> Attribute
mkErrorCode = ErrorCode

data Message = Message
    { msgType :: MessageType
    , transactionId :: TransactionId
    , attributes :: [Attribute]
    }
    deriving (Eq, Show)

mkMessage :: MessageType -> TransactionId -> [Attribute] -> Message
mkMessage = Message
