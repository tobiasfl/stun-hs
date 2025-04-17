module Network.STUN.Types
    (Message
    , mkMessage
    , mkClassicMessage
    , msgType
    , transactionId
    , attributes
    , isClassic
    , MessageType(..)
    , TransactionId(..)
    , Address(..)
    , Attribute(..)
    , ErrCode(..)
    , mkMappedAddress
    , mkXORMappedAddress
    , mkErrorCode
    , mkAddress
    , mkIPv4Address
    , mkIPv6Address)
    where

import qualified Network.Socket  as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Word as W

newtype TransactionId = TransactionId BS.ByteString
    deriving (Eq, Show)

data MessageType =
    BindingRequest
  | BindingSuccessResponse
  | BindingErrorResponse
  deriving (Eq, Show, Enum, Bounded)

data Address = IPv4 S.PortNumber S.HostAddress | IPv6 S.PortNumber S.HostAddress6
    deriving (Eq, Show)

mkAddress :: S.SockAddr -> Maybe Address
mkAddress (S.SockAddrInet port hostAddr) = Just $ IPv4 port hostAddr
mkAddress (S.SockAddrInet6 port _ hostAddr _) = Just $ IPv6 port hostAddr
mkAddress _ = Nothing

mkIPv4Address :: S.PortNumber -> S.HostAddress -> Address
mkIPv4Address = IPv4

mkIPv6Address :: S.PortNumber -> S.HostAddress6 -> Address
mkIPv6Address = IPv6

data ErrCode =
    TryAlternate300
  | BadRequest400
  | Unauthenticated401
  | UnknownAttribute420
  | StaleNonce438
  | ServerError500
  deriving (Eq, Show, Enum, Bounded)

data Attribute =
    MappedAddress Address
  | XORMappedAddress Address
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
  | UnknownAttributes [W.Word16]
--  | Software
--  | AlternateServer
--  | AlternateDomain
  | UnknownComprehensionRequired W.Word16
    deriving (Eq, Show)

mkMappedAddress :: Address -> Attribute
mkMappedAddress = MappedAddress

mkXORMappedAddress :: Address -> Attribute
mkXORMappedAddress = XORMappedAddress

mkErrorCode :: ErrCode -> T.Text -> Attribute
mkErrorCode = ErrorCode

data Message = Message
    { msgType :: MessageType
    , transactionId :: TransactionId
    , attributes :: [Attribute]
    , isClassic :: Bool
    }
    deriving (Eq, Show)

mkMessage :: MessageType -> TransactionId -> [Attribute] -> Message
mkMessage mType tid attrs = Message mType tid attrs False

mkClassicMessage :: MessageType -> TransactionId -> [Attribute] -> Message
mkClassicMessage mType tid attrs = Message mType tid attrs True
