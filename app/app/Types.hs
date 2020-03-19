{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import qualified Data.ByteString.Lazy    as BL
import           Data.Csv                (DefaultOrdered (headerOrder),
                                          FromField (parseField),
                                          FromNamedRecord (parseNamedRecord),
                                          Header, ToField (toField),
                                          ToNamedRecord (toNamedRecord), (.:),
                                          (.=))
import qualified Data.Csv                as Csv
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.Vector             as V

--------------------------------------------------------------------------------

-- simple table that used for translation
data Translation =
  Translation
   { key    :: T.Text  -- xml key
   , langEn :: T.Text
   , langDa :: T.Text
   , langNo :: T.Text
   , langSe :: T.Text
   } deriving (Eq, Show)

instance FromNamedRecord Translation where
  parseNamedRecord m =
    Translation
      <$> m Csv..: "key"
      <*> m Csv..: "en"
      <*> m Csv..: "da"
      <*> m Csv..: "no"
      <*> m Csv..: "sv"

instance ToNamedRecord Translation where
  toNamedRecord (Translation {..}) =
    Csv.namedRecord
      [ "key"       Csv..= key
      , "en"        Csv..= langEn
      , "da"        Csv..= langDa
      , "no"        Csv..= langNo
      , "sv"        Csv..= langSe
      ]

--------------------------------------------------------------------------------

readData :: String -> IO (Either String [Translation])
readData fname =
  do
    csvData <- BL.readFile fname
    case TEL.decodeUtf8' csvData of
      Left  err  -> do
        putStrLn $ "error decoding" ++ (show err)
        return $ Left $ show err
      Right dat  -> do
        let dat' = dat
        case decodeTranslations$ TEL.encodeUtf8 dat' of
          Left  err  -> do
            putStrLn $ "error decoding" ++ (show err)
            return $  Left err
          Right vals ->
            do
              -- putStrLn $ show $ vals
              return $ Right (V.toList vals)


decodeTranslations :: BL.ByteString -> Either String (V.Vector Translation)
decodeTranslations = fmap snd . Csv.decodeByName
