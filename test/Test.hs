{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import ScopedInstances

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "GenericEncode" $ do
    it "should use the default instances" $ do
      let r = Rec1 12 "hi" "bye"
      encode r `shouldBe` "foo = 12, bar = hi, baz = bye"

  describe "GenericEncode `Using` Encode (Text `As` Uptext)" $ do
    it "should use the Uptext instance instead of Text" $ do
      let r = Rec2 12 "hi" "bye"
      encode r `shouldBe` "foo = 12, bar = HI, baz = BYE"

data Rec1 = Rec1
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (Encode) via GenericEncode Rec1

uptextEncoder :: Encoder Text
uptextEncoder = Encoder Text.toUpper

newtype Uptext = Uptext { unUptext :: Text }

instance Encode Uptext where
  encode = runEncoder uptextEncoder . unUptext

data Rec2 = Rec2
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
             -- TODO: via GenericEncode Rec2 `Using` Encode (Text `As` Uptext)
    deriving (Encode) via GenericEncode Rec2
