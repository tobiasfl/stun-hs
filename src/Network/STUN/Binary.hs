{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.STUN.Binary
    (serializeMessage
    , deserializeMessage
    , xorAddress)
    where

import qualified Network.STUN.Types as Types

import Data.Bits
import Data.Word
import Data.Serialize
import Data.Either
import Control.Monad
import qualified Data.ByteString as BS
import Network.Socket (PortNumber, HostAddress6, HostAddress)
import Control.Applicative
import qualified Data.Text.Encoding as TE

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

putMessageTypeField :: Types.MessageType -> Put
putMessageTypeField msgType = putWord16be $ setMethodField $ setClassField 0x0000
    where setClassField word = let c0 = 4
                                   c1 = 8
                                    in case msgType of
                                      Types.BindingRequest -> word
                                      Types.BindingSuccessResponse -> setBit word c1
                                      Types.BindingErrorResponse -> foldl setBit word [c0, c1]
          setMethodField = let m0 = 0 in (`setBit` m0)

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

getMappedAddressVal :: Get Types.Address
getMappedAddressVal = do
    lengthInBytes <- getWord16be
    when (((fromIntegral lengthInBytes `mod` 4) :: Int) /= 0) $ fail "Attribute length not 32-bit aligned"
    guard (lengthInBytes `elem` [8, 20])
    getWord8 >>= guard . (== 0x00)
    addrFamilyByte <- getWord8
    port <- getPort
    case addrFamilyByte of
                 0x01 -> Types.IPv4 port <$> getWord32be
                 0x02 -> Types.IPv6 port <$> getHostAddress6
                 _ -> fail "Invalid address family"

getMappedAddres :: Get Types.Attribute
getMappedAddres = do
    attributeType <- getWord16be
    when (attributeType /= 0x0001) $ fail "Invalid attribute type"
    Types.MappedAddress <$> getMappedAddressVal

getXORMappedAddres :: Get Types.Attribute
getXORMappedAddres = do
    attributeType <- getWord16be
    when (attributeType /= 0x0020) $ fail "Invalid attribute type"
    Types.XORMappedAddress <$> getMappedAddressVal

getErrorCode :: Get Types.Attribute
getErrorCode = do
    attributeType <- getWord16be
    when (attributeType /= 0x0009) $ fail "Invalid attribute type"
    lengthInBytes <- getWord16be
    when (((fromIntegral lengthInBytes `mod` 4) :: Int) /= 0) $ fail "Attribute length not 32-bit aligned"
    skip 2
    errCodeHundredsDigit <- (0x07 .&.) <$> getWord8
    unless (errCodeHundredsDigit `elem` [3..6]) $ fail "Error code must be between 300-600"
    number <- (`mod` 100) <$> getWord8
    reasonPhrase <- TE.decodeUtf8 <$> getByteString (fromIntegral lengthInBytes - 4)
    let (errCodeNumber :: Int) = (fromIntegral errCodeHundredsDigit * 100) + fromIntegral number
    errCode <- case errCodeNumber of
                 300 -> pure Types.TryAlternate300
                 400 -> pure Types.BadRequest400
                 401 -> pure Types.Unauthenticated401
                 420 -> pure Types.UnknownAttribute420
                 438 -> pure Types.StaleNonce438
                 500 -> pure Types.ServerError500
                 _ -> fail "Error code invalid or not implemented"
    pure $ Types.ErrorCode errCode reasonPhrase

getAttribute :: Get Types.Attribute
getAttribute = do
    getMappedAddres <|> getXORMappedAddres <|> getErrorCode

w32ToBSBE :: Word32 -> BS.StrictByteString
w32ToBSBE w = runPut $ putWord32be w

w16ToBSBE :: Word16 -> BS.StrictByteString
w16ToBSBE w = runPut $ putWord16be w

addressToByteString :: Types.Address -> BS.StrictByteString
addressToByteString (Types.IPv4 port addr) = BS.pack [0x00, 0x01] <> w16ToBSBE (fromIntegral port) <> encode addr
addressToByteString (Types.IPv6 port (a, b, c, d)) = BS.pack [0x00, 0x02] <> w16ToBSBE (fromIntegral port) <> (BS.concat $ fmap w32ToBSBE  [a, b, c, d] :: BS.StrictByteString)

attributeToByteString :: Types.Attribute -> BS.StrictByteString
attributeToByteString (Types.ErrorCode code reason) = BS.pack [0x00, 0x09] <> w16ToBSBE (fromIntegral (BS.length val)) <> val
    where val = BS.pack ([0x00, 0x00] <> [0x07 .&. hundreds, num]) <> TE.encodeUtf8 reason
          (hundreds, num) = case code of
                              Types.TryAlternate300 -> (3, 0)
                              Types.BadRequest400 -> (4, 0)
                              Types.Unauthenticated401 -> (4, 1)
                              Types.UnknownAttribute420 -> (4, 20)
                              Types.StaleNonce438 -> (4, 38)
                              Types.ServerError500 -> (5, 0)
attributeToByteString mappedAddr = case mappedAddr of
                                     (Types.MappedAddress addr) -> BS.pack [0x00, 0x01] <> lenAndBody addr
                                     (Types.XORMappedAddress addr) -> BS.pack [0x00, 0x20] <> lenAndBody addr
                                 where lenAndBody addr = let val = addressToByteString addr in w16ToBSBE (fromIntegral (BS.length val)) <> val

data Message = Message Types.MessageType Types.TransactionId [Types.Attribute]

instance Serialize Message where
    put (Message msgType (Types.TransactionId tid) attributes) = do
        putMessageTypeField msgType
        let attributesBytesTring = BS.concat $ fmap attributeToByteString attributes
        putWord16be $ fromIntegral $ BS.length attributesBytesTring
        putWord32be magicCookie
        putByteString tid
        putByteString attributesBytesTring

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
        attributes <- getAttributesUntilEnd [] remainingByteLen
        pure $ Message msgType (Types.TransactionId tid) (reverse attributes)
            where getAttributesUntilEnd attrs remainingBytes
                    | remainingBytes > 0 = do
                        a <- getAttribute
                        r <- remaining
                        getAttributesUntilEnd (a:attrs) r
                    | remainingBytes == 0 = pure attrs
                    | otherwise = fail "Failed to get attributes"


serializeMessage :: Types.Message -> BS.StrictByteString
serializeMessage msg = encode ((Message <$> Types.msgType <*> Types.transactionId <*> Types.attributes) msg)

deserializeMessage :: BS.StrictByteString -> Either String Types.Message
deserializeMessage bs = decode bs >>= \(Message msgType tid attrs) -> pure $ Types.mkMessage msgType tid attrs

xorPortNumber :: PortNumber -> PortNumber
xorPortNumber port = fromIntegral $ toInteger port `xor` (toInteger magicCookie `shiftR` 16)

xorHostAddress :: HostAddress -> HostAddress
xorHostAddress addr = fromIntegral $ toInteger addr `xor` toInteger magicCookie

bSBEToW32 :: BS.StrictByteString -> Word32
bSBEToW32 bs = fromRight 0 $ runGet getWord32be bs

xorHostAddress6 :: Types.TransactionId -> HostAddress6 -> HostAddress6
xorHostAddress6 (Types.TransactionId tid) (a, b, c, d) = (a `xor` magicCookie, xorB, xorC, xorD)
    where bcdXor = BS.zipWith xor (BS.concat $ fmap w32ToBSBE [b, c, d]) tid
          xorB = bSBEToW32 $ BS.pack bcdXor
          xorC = bSBEToW32 $ BS.pack $ drop 4 bcdXor
          xorD = bSBEToW32 $ BS.pack $ drop 8 bcdXor

xorAddress :: Types.TransactionId -> Types.Address -> Types.Address
xorAddress _ (Types.IPv4 port addr) = Types.IPv4 (xorPortNumber port) (xorHostAddress addr)
xorAddress tid (Types.IPv6 port addr) = Types.IPv6 (xorPortNumber port) (xorHostAddress6 tid addr)
