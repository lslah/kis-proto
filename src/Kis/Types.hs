{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Kis.Types
  ( Bed(..)
  , BedId(..)
  , BedSubmit(..)
  , Patient(..)
  , PatientId(..)
  , PatientSubmit(..)
  , KisConversion(..)
  )
where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import GHC.Generics

data Bed =
    Bed
    { b_id :: BedId
    , b_name :: String
    } deriving (Show, Generic)

instance ToJSON Bed

data Patient =
    Patient
    { p_id :: PatientId
    , p_name :: String
    } deriving (Show, Generic, Eq)

instance FromJSON Patient
instance ToJSON Patient

data PatientSubmit = PatientSubmit { ps_name :: String }
    deriving (Show, Generic)

instance FromJSON PatientSubmit
instance ToJSON PatientSubmit

data BedSubmit = BedSubmit { bs_name :: String }
    deriving (Show, Generic)

instance FromJSON BedSubmit

newtype BedId = BedId Integer
    deriving (Eq, Ord, FromField, ToField, Generic)

instance ToJSON BedId

newtype PatientId = PatientId Integer
    deriving (Eq, Ord, FromField, ToField, Generic)

instance FromJSON PatientId
instance ToJSON PatientId

instance FromRow Bed where
  fromRow = Bed <$> field <*> field

instance FromRow Patient where
  fromRow = Patient <$> field <*> field

instance Show BedId where show (BedId idx) = show idx
instance Show PatientId where show (PatientId idx) = show idx

class KisConversion a b | a -> b where
  toKis :: b -> a
  fromKis :: a -> b
