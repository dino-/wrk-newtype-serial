{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module Newtype.Serial.Month
   ( Month (unMonth)
   , mkMonth, mkMonthE
   )
   where

import Control.Monad.Except
import Data.Aeson
import Debug.Trace ( trace )
import GHC.Generics
import Text.Printf ( printf )
import Text.Read ( readMaybe )


january, december :: Int
january = 0
december = 11


newtype Month = Month { unMonth :: Int }
   deriving Generic

instance FromJSON Month

instance ToJSON Month

instance Read Month where
   readsPrec d s =
      filter ((either (const False) (const True)) . validateMonth . fst)
         [ (Month i, s') | (i, s') <- readsPrec d s ]

instance Show Month where
   show (Month o) = show o


{- We don't have dependent types and so can't express a subset of
   the Int domain in our newtype. But we can at least provide a way
   to validate these values with meaningful error messages.
-}
validateMonth :: Month -> Either String Month
validateMonth i@(Month n)
   | (january <= n) && (n <= december) = Right i
   | otherwise = Left $
      printf "Month is invalid because %d is not between %d and %d"
         n january december


mkMonth :: Int -> Either String Month
mkMonth = validateMonth . Month


mkMonthE :: (MonadError String m) => Int -> m Month
mkMonthE = either throwError return . mkMonth
