{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)

data Scientist = Scientist
  { sId        :: Int
  , sNames     :: [String]
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Scientist)

scientists :: [Scientist]
scientists = [ Scientist 1 ["Isaac", "Newton"]
             , Scientist 2 ["Albert", "Einstein"]
             , Scientist 3 ["Gottfried", "Wilhelm", "Leibniz"]
             , Scientist 4 ["Stephen", "Hawking"]
             , Scientist 5 ["Pythagoras"]
             , Scientist 6 ["Wernher", "von", "Braun"]
             ]
