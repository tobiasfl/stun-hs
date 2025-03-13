{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.STUN.Binary
    (serializeMessage
    , deserializeMessage)
    where

import qualified Network.STUN.Types as Types

import Data.Bits
import Data.Word
import Data.Serialize 
--import Debug.Trace (trace)
import Control.Monad 
import qualified Data.ByteString as BS
import Network.Socket (PortNumber, HostAddress6)
import Control.Applicative

--     https://datatracker.ietf.org/doc/html/rfc8489
--
--      0                   1                   2                   3
--      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     |0 0|     STUN Message Type     |         Message Length        |
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     |                         Magic Cookie                          |
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     |                                                               |
--     |                     Transaction ID (96 bits)                  |
--     |                                                               |
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     STUN Message Header
--
--                       0                 1
--                       2  3  4 5 6 7 8 9 0 1 2 3 4 5
--                      +--+--+-+-+-+-+-+-+-+-+-+-+-+-+
--                      |M |M |M|M|M|C|M|M|M|C|M|M|M|M|
--                      |11|10|9|8|7|1|6|5|4|0|3|2|1|0|
--                      +--+--+-+-+-+-+-+-+-+-+-+-+-+-+
--                      STUN Message Type Field
--
--      0                   1                   2                   3
--      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     |         Type                  |            Length             |
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     |                         Value (variable)                ....
--     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--     STUN Attributes

data ClassField = Request | Indication | SuccessResponse | ErrorResponse
    deriving (Eq, Show)

data MethodField = Binding
    deriving (Eq, Show)

type MessageTypeField = (ClassField, MethodField)

getMessageTypeField :: Get MessageTypeField
getMessageTypeField = do
        bytes <- getWord16be
        when (any (bytes `testBit`) [14, 15]) $ fail "Two first bits are not zeroes"
        classField <- case (bytes `testBit` 8, bytes `testBit` 4) of
                           (False, False) -> pure Request
                           (False, True) -> pure Indication
                           (True, False) -> pure SuccessResponse
                           (True, True) -> pure ErrorResponse
        let methodFieldBytes = (bytes .&. 0x3e00) .|. ((bytes .&. 0x00e0) `shiftR` 1) .|. (bytes .&. 0x000f)
        case methodFieldBytes of
          0x0001 -> pure (classField, Binding)
          _ -> fail "Invalid method or not implemented"

magicCookie :: Word32
magicCookie = 0x2112A442

getPort :: Get PortNumber
getPort = fromIntegral <$> getWord16be

getHostAddress6 :: Get HostAddress6
getHostAddress6 = do
    address <- replicateM 4 getWord32be
    case address of
      [a, b, c, d] -> pure (a, b, c, d)
      _ -> fail "HostAddress6 of invalid length"

getMappedAddres :: Get Types.Attribute
getMappedAddres = do
    attributeType <- getWord16be
    when (attributeType /= 0x0001) $ fail "Invalid attribute type"
    lengthInBytes <- getWord16be
    when (((fromIntegral lengthInBytes `mod` 4) :: Int) /= 0) $ fail "Attribute length not 32-bit aligned"
    guard (lengthInBytes `elem` [8, 20])
    getWord8 >>= guard . (== 0x00)
    addrFamilyByte <- getWord8
    port <- getPort
    address <- case addrFamilyByte of
                 0x01 -> Types.IPv4 <$> getWord32be
                 0x02 -> Types.IPv6 <$> getHostAddress6
                 _ -> fail "Invalid address family"

    pure $ Types.MappedAddress port address 

getXORMappedAddres :: Get Types.Attribute
getXORMappedAddres = do
    attributeType <- getWord16be
    when (attributeType /= 0x0020) $ fail "Invalid attribute type"
    lengthInBytes <- getWord16be
    when (((fromIntegral lengthInBytes `mod` 4) :: Int) /= 0) $ fail "Attribute length not 32-bit aligned"
    guard (lengthInBytes `elem` [8, 20])
    getWord8 >>= guard . (== 0x00)
    addrFamilyByte <- getWord8
    port <- getPort
    address <- case addrFamilyByte of
                 0x01 -> Types.IPv4 <$> getWord32be
                 0x02 -> Types.IPv6 <$> getHostAddress6
                 _ -> fail "Invalid address family"

    pure $ Types.XORMappedAddress port address 

getAttribute :: Get Types.Attribute
getAttribute = do
    getMappedAddres <|> getXORMappedAddres

data Message = Message Types.MessageType Types.TransactionId [Types.Attribute]

instance Serialize Message where
    put = undefined
    get = do
        msgTypeFields <- getMessageTypeField
        msgType <- case msgTypeFields of
                        (Request, Binding) -> pure Types.BindingRequest
                        (SuccessResponse, Binding) -> pure Types.BindingSuccessResponse
                        (ErrorResponse, Binding) -> pure Types.BindingErrorResponse
                        (Indication, Binding) -> fail "Binding indication not implemented"
        msgLength <- getWord16be
        cookie <- getWord32be
        when (cookie /= magicCookie) $ fail "Invalid magic cookie"
        tid <- getByteString 12
        remainingByteLen <- remaining
        when (remainingByteLen /= fromIntegral msgLength) $ fail "Invalid length"
        attributes <- if remainingByteLen > 0 then replicateM 1 getAttribute else pure []
        pure $ Message msgType (Types.TransactionId tid) attributes

serializeMessage :: Types.Message -> BS.StrictByteString
serializeMessage = undefined

deserializeMessage :: BS.StrictByteString -> Either String Types.Message
deserializeMessage bs = decode bs >>= \(Message msgType tid attrs) -> pure $ Types.mkMessage msgType tid attrs
