{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.Except
import Data.Aeson
import Data.Either ( lefts )
import Data.String.Conv ( toS )
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics
import Text.Read ( readEither )

import Newtype.Serial.Month


data ContainerRead = ContainerRead
   { alpha :: Month
   }
   deriving (Read, Show)


data ContainerJSON = ContainerJSON
   { beta :: Month
   }
   deriving (Generic, Show)

instance FromJSON ContainerJSON

instance ToJSON ContainerJSON


main :: IO ()
main = do
   simpleConstruction
   readMonth
   maybeReadMonth
   readContainer
   jsonContainerOut
   jsonContainerExcept


simpleConstruction :: IO ()
simpleConstruction = do
   putStrLn "Simple construction with Int"
   print . mkMonth $ 3
   print . mkMonth $ 1000


readMonth :: IO ()
readMonth = do
   putStrLn "\nreadEither with String values"
   print $ mkMonth =<< readEither "10"
   print $ mkMonth =<< readEither "-5"
   print $ mkMonth =<< readEither "shit!"


maybeReadMonth :: IO ()
maybeReadMonth = do
   putStrLn "\nreadEither and then converting to Maybe Month"
   print . either (const Nothing) (Just . unMonth) $ mkMonth =<< readEither "10"
   print . either (const Nothing) (Just . unMonth) $ mkMonth =<< readEither "-5"
   print . either (const Nothing) (Just . unMonth) $ mkMonth =<< readEither "shit!"


readContainer :: IO ()
readContainer = do
   putStrLn "\nEmbedded in a serialized data structure"
   print $ ((readEither "ContainerRead { alpha = 5 }") :: Either String ContainerRead)
   print $ ((readEither "ContainerRead { alpha = 1001 }") :: Either String ContainerRead)


jsonContainerOut :: IO ()
jsonContainerOut = do
   putStrLn "\nJSON container out"
   TL.putStrLn . TL.decodeUtf8 . either toS encode . mkMonth $ 5
   TL.putStrLn . TL.decodeUtf8 . either toS encode $ (ContainerJSON <$> mkMonth 5)
   print $ lefts [ mkMonth 5 ]


jsonContainerExcept :: IO ()
jsonContainerExcept = do
   putStrLn "\nJSON container with exceptions"
   print . runExcept $ mkMonthE 1001
   print . runExcept $ mkMonthE 5
