-- | Specifications for the Data.Currency module
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.CurrencySpec
  ( spec
  ) where

import           Data.Bson           (Val (..))
import           Test.Hspec          (Spec, describe, it)
import           Test.QuickCheck     (Arbitrary (..), property)
import           Test.QuickCheck.Gen (chooseAny)

import qualified Data.Aeson          as Aeson

import           Data.Currency       (Alpha, Currency (..), fromAlpha)


instance Arbitrary Currency where
  arbitrary =
    chooseAny


instance Arbitrary Alpha where
  arbitrary =
    chooseAny


spec :: Spec
spec = do
  describe "Round-up properties (Currency)" $ do
    it "Read . Show = id" $ property $
      \x -> (read . show) x == (x :: Currency)

    it "FromJSON . ToJSON = pure" $ property $
      \x -> (Aeson.eitherDecode . Aeson.encode) x == pure (x :: Currency)

    it "cast' . val === pure" $ property $
      \x -> (cast' . val) x == pure (x :: Currency)


  describe "Round-up properties (Alpha)" $ do
    it "Read . Show = id" $ property $
      \x -> (read . show) x == (x :: Alpha)

    it "FromJSON . ToJSON = pure" $ property $
      \x -> (Aeson.eitherDecode . Aeson.encode) x == pure (x :: Alpha)

    it "cast' . val === pure" $ property $
      \x -> (cast' . val) x == pure (x :: Alpha)


  describe "fromAlpha" $
    it "alpha . fromAlpha = id" $ property $
      \x -> (alpha . fromAlpha) x == x
