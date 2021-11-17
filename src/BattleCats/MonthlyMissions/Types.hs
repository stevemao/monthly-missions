{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BattleCats.MonthlyMissions.Types where

import           Data.List.NonEmpty
import qualified Data.Text                        as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Exts                         (IsString (..))
import           Lens.Micro.Platform

newtype StageName
  = StageName T.Text
  deriving (Eq, FromField, Show)

newtype Energy
  = Energy Int
  deriving (Eq, FromField, Num, Ord, Show)

newtype HpSpawn
  = HpSpawn T.Text
  deriving (Eq, FromField, Show)

newtype FirstSpawn
  = FirstSpawn Int
  deriving (Eq, FromField, Ord, Show)

newtype IsBoss
  = IsBoss Int
  deriving (Eq, FromField, Show)

newtype Level
  = Level T.Text
  deriving (Eq, IsString, Show)

newtype DBLevel
  = DBLevel T.Text
  deriving (Eq, FromField, IsString, Show, ToField)

data FromRowStage
  = FromRowStage Category DBLevel StageName Energy HpSpawn FirstSpawn IsBoss
  deriving (Show)

data AdjustedFromRowStage
  = AdjustedFromRowStage Category Level StageName Energy HpSpawn FirstSpawn IsBoss

instance FromRow FromRowStage where
  fromRow = FromRowStage <$> field <*> field <*> field <*> field <*> field <*> field <*> field

newtype EnemyCode
  = EnemyCode Int
  deriving (Eq, Show)

newtype Target
  = Target T.Text
  deriving (Eq, Show)

newtype Category
  = Category T.Text
  deriving (Eq, FromField, IsString, Show, ToField)

data Location
  = LocationLevel Level
  | LocationCategory Category
  deriving (Show)

data Mission
  = Mission Location Target
  deriving (Show)

newtype EnemyUnitsTSV
  = EnemyUnitsTSV T.Text

data Enemy
  = Enemy HpSpawn FirstSpawn Target EnemyCode IsBoss
  deriving (Eq, Show)

newtype FastestEnemy
  = FastestEnemy Enemy

instance Eq FastestEnemy where
  FastestEnemy (Enemy _ firstSpawnA _ _ _) == FastestEnemy (Enemy _ firstSpawnB _ _ _) =
      firstSpawnA == firstSpawnB

instance Ord FastestEnemy where
  FastestEnemy (Enemy _ firstSpawnA _ _ _) <= FastestEnemy (Enemy _ firstSpawnB _ _ _) =
      firstSpawnA <= firstSpawnB

data Stage
  = Stage Category Level StageName Energy
  deriving (Eq, Show)

getEnergy :: NonEmpty Stage -> Energy
getEnergy ss = sum $ (\(Stage _ _ _ e) -> e) <$> ss

getEnergy' :: NonEmpty StageWithEnemy -> Energy
getEnergy' stages = getEnergy ((^. _1) <$> stages)

type StageWithEnemy = (Stage, NonEmpty Enemy)

newtype MinEnergyStages
  = MinEnergyStages (NonEmpty StageWithEnemy)

instance Eq MinEnergyStages where
  MinEnergyStages stagesA == MinEnergyStages stagesB =
      getEnergy' stagesA == getEnergy' stagesB

instance Ord MinEnergyStages where
  MinEnergyStages stagesA <= MinEnergyStages stagesB =
      getEnergy' stagesA <= getEnergy' stagesB

newtype StageNames
  = StageNames T.Text
  deriving (Eq, FromField)

data FromRowStageCategory
  = FromRowStageCategory StageNames DBLevel Category
  deriving (Eq)

instance FromRow FromRowStageCategory where
  fromRow = FromRowStageCategory <$> field <*> field <*> field

data AggregatedStages
  = AggregatedStages (NonEmpty StageName) DBLevel Category
  deriving (Eq)

newtype Map
  = Map (NonEmpty (NonEmpty Bool))

type StageWithEnemyMap = (Stage, NonEmpty Enemy, Map)

newtype MinEnergyStagesWithMap
  = MinEnergyStagesWithMap (NonEmpty StageWithEnemyMap)
