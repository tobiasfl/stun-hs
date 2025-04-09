{-# LANGUAGE OverloadedStrings #-}

module Network.STUN.BinarySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Network.STUN.Binary as Binary
import qualified Network.STUN.Types as Types
import qualified Data.ByteString as BS
import Data.Either (fromLeft)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Network.STUN.BinaryUtils

stunBindRequestInvalidMagicCookie :: BS.StrictByteString
stunBindRequestInvalidMagicCookie = bindRequest <> BS.pack [0x00, 0x00]
    <> BS.pack [0x21, 0x12, 0xa5, 0x42] <>
    BS.pack [0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a]

newtype ArbitraryAttribute = Attr {unwrap :: Types.Attribute}
    deriving (Eq, Show)

instance Arbitrary ArbitraryAttribute where
  arbitrary = Attr <$> oneof [mappedAddressGen, errorCodeGen, unknownAttributesGen]
    where portGen = arbitraryBoundedEnum
          ipv4AddressGen = Types.IPv4 <$> portGen <*> arbitrary
          ipv6AddressGen = Types.IPv6 <$> portGen <*> arbitrary
          mappedAddressGen = do
              ctor <- elements [Types.MappedAddress, Types.XORMappedAddress]
              addr <- oneof [ipv4AddressGen, ipv6AddressGen]
              pure $ ctor addr
          errorCodeGen = Types.ErrorCode <$> arbitraryBoundedEnum <*> (arbitrary <&> T.pack)
          unknownAttributesGen = Types.UnknownAttributes <$> listOf1 (choose (0x021, 0x7FFF))

newtype ArbitraryMessage = Msg Types.Message
    deriving (Eq, Show)

instance Arbitrary ArbitraryMessage where
  arbitrary = do
      msgType <- arbitraryBoundedEnum
      tid <- Types.TransactionId . BS.pack <$> vectorOf 12 arbitrary
      attributes <- listOf arbitrary
      pure $ Msg $ Types.mkMessage msgType tid (fmap unwrap attributes)
  shrink (Msg msg) = msgWithAttributes . fmap unwrap <$> shrunkAttributePerms
      where shrunkAttributePerms = shrink (Attr <$> Types.attributes msg)
            msgWithAttributes = Msg . Types.mkMessage (Types.msgType msg) (Types.transactionId msg)

spec :: Spec
spec = do
  describe "Message encoding/decoding" $ do
    it "Decodes a STUN bind request correctly" $ do
      let msg = Binary.deserializeMessage $ stunBindRequest 0x0
      let expectedTransactionId = Types.TransactionId $ BS.pack [ 0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a ]
      let expectedMsg = Types.mkMessage Types.BindingRequest expectedTransactionId []
      msg `shouldBe` Right expectedMsg
    it "Fails to decode when invalid magic cookie" $ do
      let msg = Binary.deserializeMessage stunBindRequestInvalidMagicCookie
      fromLeft "" msg `shouldContain` "Invalid magic cookie"
    it "Fails to decode when invalid length" $ do
      let stunBindWithInvalidLength = stunBindRequest 0x10
      let msg = Binary.deserializeMessage stunBindWithInvalidLength
      fromLeft "" msg `shouldContain` "Invalid length"
    it "Decodes a STUN bind success response correctly" $ do
      let msg = Binary.deserializeMessage $ stunBindSuccessResponse 12 <> mappedAddressAttribute
      let expectedTransactionId = Types.TransactionId $ BS.pack [ 0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a ]
      Types.msgType <$> msg `shouldBe` Right Types.BindingSuccessResponse
      Types.transactionId <$> msg `shouldBe` Right expectedTransactionId
    context "When decoding a STUN bind request with attributes" $ do
      it "Decodes unknown attributes of type between 0x0000 and 0x7FFF as comprehension-required" $ do
        let msg = Binary.deserializeMessage $ stunBindRequest 8 <> BS.pack [0x00, 0x2b, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00]
        let expectedAttribute = Types.UnknownComprehensionRequired 0x002b
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
      it "Decodes comprehension-required attribute with padding correctly" $ do
        let padding = BS.pack [0x00, 0x00]
        let msg = Binary.deserializeMessage $ stunBindRequest 8 <> BS.pack [0x00, 0x2b, 0x00, 0x02, 0x01, 0x02] <> padding
        let expectedAttribute = Types.UnknownComprehensionRequired 0x002b
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
      it "Returns an error for a non-32 bit aligned unknown comprehension-required attribute" $ do
        let msg = Binary.deserializeMessage $ stunBindRequest 9 <> BS.pack [0x00, 0x2b, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00]
        fromLeft "" msg `shouldContain` "Message length not 32-bit aligned"
    context "When decoding a STUN bind success response with attributes" $ do
      it "Decodes a MappedAddress with IPv4 address attribute correctly" $ do
        let msg = Binary.deserializeMessage $ stunBindSuccessResponse 12 <> mappedAddressAttribute
        let expectedAttribute = Types.mkMappedAddress $ Types.IPv4 4604  0x46c7802e
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
      it "Decodes an XORMappedAddress with IPv6 address attribute correctly" $ do
        let msg = Binary.deserializeMessage $ stunBindSuccessResponse 24 <> xorMappedAddressAttribute
        let expectedAttribute = Types.mkXORMappedAddress $ Types.IPv6 4604 (0x46c7802e, 0x46c7802e, 0x46c7802e, 0x462ec780)
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
    it "Decodes a STUN bind error response correctly" $ do
      let msg = Binary.deserializeMessage $ stunBindErrorResponse 0
      let expectedTransactionId = Types.TransactionId $ BS.pack [0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a]
      let expectedMsg = Types.mkMessage Types.BindingErrorResponse expectedTransactionId []
      msg `shouldBe` Right expectedMsg
    context "When decoding a STUN bind error response with attributes" $ do
      it "Decodes a ErrorCode 500 with an reason phrase correctly" $ do
        let msg = Binary.deserializeMessage $ stunBindErrorResponse 20  <> errorCode500Attribute
        let expectedAttribute = Types.ErrorCode Types.ServerError500 "SERVER ERROR"
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
      it "Decodes a ErrorCode 500 with padding correctly" $ do
        let padding = BS.pack [0x00]
        let errCode500Attribute = BS.pack [0x00, 0x09, 0x00, 0x0F, 0x00, 0x00, 0x05, 0x00, 0x53, 0x45, 0x52, 0x56, 0x45, 0x52, 0x20, 0x45, 0x52, 0x52, 0x4f]
        let msg = Binary.deserializeMessage $ stunBindErrorResponse 20  <> errCode500Attribute <> padding
        let expectedAttribute = Types.ErrorCode Types.ServerError500 "SERVER ERRO"
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
      it "Decodes a ErrorCode 420 with a list of unknown attributes types correctly" $ do
        let padding = BS.pack [0x00, 0x00]
        let attrLength = 0x06
        let msg = Binary.deserializeMessage $ stunBindErrorResponse 20  <> errorCode420Attribute <> BS.pack [0x00, 0x0a, 0x00, attrLength, 0x7F, 0xFF, 0x6F, 0xFF, 0x05, 0xFF] <> padding
        let expectedAttributes = [Types.ErrorCode Types.UnknownAttribute420 "", Types.UnknownAttributes [0x7FFF, 0x6FFF, 0x05FF]]
        Types.attributes <$> msg `shouldBe` Right expectedAttributes

    it "Encodes a STUN bind success response correctly" $ do
      let tid = BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01]
      let msg = Types.mkMessage Types.BindingSuccessResponse (Types.TransactionId tid) []
      let result = Binary.serializeMessage msg
      result `shouldBe` BS.pack [
          0x01, 0x01,
          0x00, 0x00,
          0x21, 0x12, 0xa4, 0x42] <> tid
    context "When encoding a STUN bind response with attributes" $ do
      it "Encodes an XORMappedAddress with IPV4 address and a Unauthenticated401 error correctly" $ do
        let tid = BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01]
        let errCodeAttr = Types.mkErrorCode Types.Unauthenticated401 ""
        let msg = Types.mkMessage Types.BindingSuccessResponse (Types.TransactionId tid) [Types.mkXORMappedAddress $ Types.IPv4 4604 0x46c7802e, errCodeAttr]
        let result = Binary.serializeMessage msg
        let xorMappedAttr = BS.pack [0x00, 0x20, 0x00, 0x08, 0x00, 0x01, 0x11, 0xfc, 0x46, 0xc7, 0x80, 0x2e]
        result `shouldBe` BS.pack [
            0x01, 0x01,
            0x00, 0x14]
            <> magicCookie <> tid <> xorMappedAttr <> BS.pack [0x00, 0x09, 0x00, 0x04, 0x00, 0x00, 0x04, 0x01]
        Binary.deserializeMessage result `shouldBe` Right msg
      it "Encodes an XORMappedAddress with IPV4 address correctly" $ do
        let tid = BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01]
        let msg = Types.mkMessage Types.BindingSuccessResponse (Types.TransactionId tid) [Types.mkXORMappedAddress $ Types.IPv4 4604 0x46c7802e]
        let result = Binary.serializeMessage msg
        let xorMappedAttr = BS.pack [0x00, 0x20, 0x00, 0x08, 0x00, 0x01, 0x11, 0xfc, 0x46, 0xc7, 0x80, 0x2e]
        result `shouldBe` BS.pack [
            0x01, 0x01,
            0x00, 0x0c,
            0x21, 0x12, 0xa4, 0x42] <> tid <> xorMappedAttr
    context "When encoding a STUN bind error response with attributes" $ do
      it "Encodes a ErrorCode Unauthenticated401 correctly" $ do
        let tid = [0x01, 0x01, 0x15, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01]
        let errCodeAttr = Types.mkErrorCode Types.Unauthenticated401 ""
        let msg = Types.mkMessage Types.BindingErrorResponse (Types.TransactionId (BS.pack tid)) [errCodeAttr]
        let result = Binary.serializeMessage msg
        result `shouldBe` BS.pack ([
              0x01, 0x11,
              0x00, 0x08,
              0x21, 0x12, 0xa4, 0x42] <> tid <> [0x00, 0x09, 0x00, 0x04, 0x00, 0x00, 0x04, 0x01])

    prop "Encode then decode is equivalent to the original message" $
        \(Msg msg) -> Binary.deserializeMessage (Binary.serializeMessage msg) == Right msg

    prop "All encoded STUN messages are 32-bit aligned" $
        \(Msg msg) -> BS.length (Binary.serializeMessage msg) `mod` 4 == 0

  describe "XOR'ing IPv4 addresses" $ do
    context "When the port and address are equal to the magic cookie" $ do
      it "XOR's both the port and address to 0" $ do
        let tid = Types.TransactionId $ BS.pack [0x01, 0x01, 0x15, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01]
        let address = Types.mkIPv4Address 0x2112 0x2112A442
        let result = Binary.xorAddress tid address
        result `shouldBe` Types.mkIPv4Address 0x0 0x0
    context "When the port and address are the complement of the magic cookie" $ do
      it "XOR's both the port and address so all bits are set" $ do
        let tid = Types.TransactionId $ BS.pack [0x01, 0x01, 0x15, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01]
        let address = Types.mkIPv4Address 0xdeed 0xDEED5BBD
        let result = Binary.xorAddress tid address
        result `shouldBe` Types.mkIPv4Address 0xffff 0xffffffff
  describe "XOR'ing IPv6 addresses" $ do
    context "When the address is equal to the magic cookie concatenated with the transaction id" $ do
      it "XOR's the address to 0" $ do
        let tid = Types.TransactionId $ BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c]
        let address = Types.mkIPv6Address 0x2112 (0x2112A442, 0x01020304, 0x05060708, 0x090a0b0c)
        let result = Binary.xorAddress tid address
        result `shouldBe` Types.mkIPv6Address 0x0 (0x0, 0x0, 0x0, 0x0)
