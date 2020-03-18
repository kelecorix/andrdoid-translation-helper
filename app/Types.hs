{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T

--------------------------------------------------------------------------------

-- simple table that used for translation
data Translations =
  Translations
   { langEn :: T.Text
   , langDa :: T.Text
   , langNo :: T.Text
   , langSe :: T.Text
   } deriving (Eq, Show)
