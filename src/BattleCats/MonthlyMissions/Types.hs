{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BattleCats.MonthlyMissions.Types where

import qualified Data.Text                        as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Exts                         (IsString (..))

newtype StageName = StageName T.Text
    deriving (Show, FromField, Eq)

instance FromRow StageName where
  fromRow = StageName <$> field

newtype Energy = Energy Int
    deriving (Show, FromField, Eq, Num, Ord)

instance FromRow Energy where
  fromRow = Energy <$> field

data Stage = Stage Level StageName Energy
    deriving (Show, Eq)

instance FromRow Stage where
  fromRow = Stage <$> field <*> field <*> field

newtype EnemyCode = EnemyCode Int

newtype Level = Level T.Text
    deriving (Show, FromField, Eq, IsString, ToField)

instance FromRow Level where
  fromRow = Level <$> field

newtype Target = Target T.Text
    deriving Show

newtype Category = Category T.Text
    deriving (Show, IsString, ToField)

data Location = LocationLevel Level | LocationCategory Category
    deriving Show

data Mission = Mission Location Target
    deriving Show

newtype EnemyUnitsTSV = EnemyUnitsTSV T.Text

energy :: [Stage] -> Energy
energy = foldr (\(Stage _ _ e) -> (e +)) 0

newtype Stages = Stages [Stage]
  deriving (Eq, Show)

instance Ord Stages where
  Stages stagesA <= Stages stagesB = energy stagesA <= energy stagesB
