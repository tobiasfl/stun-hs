module Network.STUN.BinarySpec (spec) where

import Test.Hspec
--import Test.QuickCheck
import qualified Network.STUN.Binary as Binary
import qualified Network.STUN.Types as Types
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Either (fromLeft)

magicCookie :: BS.StrictByteString
magicCookie = BS.pack [0x21, 0x12, 0xa4, 0x42]

bindRequest :: BS.StrictByteString
bindRequest = BS.pack [0x00, 0x01]

stunBindRequest :: BS.StrictByteString
stunBindRequest = 
    bindRequest <>
    BS.pack [0x00, 0x00]  -- Length: 0 bytes (no attributes)
    <> magicCookie <>
    -- Transaction ID
    BS.pack [0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a
  ]

stunBindRequestInvalidMagicCookie :: BS.StrictByteString
stunBindRequestInvalidMagicCookie = BS.pack [
    0x00, 0x01,  -- Type: Binding Request (0x0001)
    0x00, 0x00,  -- Length: 0 bytes (no attributes)
    0x21, 0x12, 0xa3, 0x42,  -- Magic Cookie (fixed value)
    -- Transaction ID
    0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a
  ]

stunBindSuccessResponse :: Word8 -> BS.StrictByteString
stunBindSuccessResponse len = BS.pack [
    0x01, 0x01, -- Type
    0x00, len,
    0x21, 0x12, 0xa4, 0x42, -- Magic Cookie
    -- TransactionId
    0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a
  ]

mappedAddressAttribute :: BS.StrictByteString
mappedAddressAttribute = BS.pack [0x00, 0x01, 0x00, 0x08, 0x00, 0x01, 0x11, 0xfc, 0x46, 0xc7, 0x80, 0x2e]

xorMappedAddressAttribute :: BS.StrictByteString
xorMappedAddressAttribute = BS.pack [0x00, 0x20, 0x00, 0x14, 0x00, 0x02, 0x11, 0xfc, 0x46, 0xc7, 0x80, 0x2e, 0x46, 0xc7, 0x80, 0x2e, 0x46, 0xc7, 0x80, 0x2e, 0x46, 0x2e, 0xc7, 0x80]

spec :: Spec
spec = do
  describe "Binary encoding/decoding" $ do
    it "Decodes a STUN bind request correctly" $ do
      let msg = Binary.deserializeMessage stunBindRequest
      let expectedTransactionId = Types.TransactionId $ BS.pack [ 0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a ]
      let expectedMsg = Types.mkMessage Types.BindingRequest expectedTransactionId []
      msg `shouldBe` Right expectedMsg
    it "Fails to decode when invalid magic cookie" $ do
      let msg = Binary.deserializeMessage stunBindRequestInvalidMagicCookie
      fromLeft "" msg `shouldContain` "Invalid magic cookie"
    it "Fails to decode when invalid length" $ do
      let stunBindWithInvalidLength = bindRequest <> BS.pack [0x0f] <> BS.drop 3 stunBindRequest
      let msg = Binary.deserializeMessage stunBindWithInvalidLength
      fromLeft "" msg `shouldContain` "Invalid length"
    it "Decodes a STUN bind success response correctly" $ do
      let msg = Binary.deserializeMessage $ stunBindSuccessResponse 12 <> mappedAddressAttribute
      let expectedTransactionId = Types.TransactionId $ BS.pack [ 0x53, 0x4f, 0x70, 0x43, 0x69, 0x69, 0x35, 0x4a, 0x66, 0x63, 0x31, 0x7a ]
      Types.msgType <$> msg `shouldBe` Right Types.BindingSuccessResponse
      Types.transactionId <$> msg `shouldBe` Right expectedTransactionId
    context "When decoding a STUN bind success response with attributes" $ do
      it "Decodes a MappedAddress with IPv4 address attribute correctly" $ do
        let msg = Binary.deserializeMessage $ stunBindSuccessResponse 12 <> mappedAddressAttribute
        let expectedAttribute = Types.MappedAddress 4604 $ Types.IPv4 0x46c7802e
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]
      it "Decodes an XORMappedAddress with IPv6 address attribute correctly" $ do
        let msg = Binary.deserializeMessage $ stunBindSuccessResponse 24 <> xorMappedAddressAttribute
        let expectedAttribute = Types.XORMappedAddress 4604 $ Types.IPv6 (0x46c7802e, 0x46c7802e, 0x46c7802e, 0x462ec780)
        Types.attributes <$> msg `shouldBe` Right [expectedAttribute]


    --TODO: quickcheck test that checks msg -> encode -> decode == msg
    --it "handles binary representation properly" $ property $
    --  \msg -> decode (encode msg) == Right (msg :: StunMessage)
    --
    ---- Add more specific tests for your Binary module functions
