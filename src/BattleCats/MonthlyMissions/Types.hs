{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BattleCats.MonthlyMissions.Types where

import           Data.List.NonEmpty
import qualified Data.Text                        as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Exts                         (IsString (..))

newtype StageName
  = StageName T.Text
  deriving (Eq, FromField, Ord, Show)

instance FromRow StageName where
  fromRow = StageName <$> field

newtype Energy
  = Energy Int
  deriving (Eq, FromField, Num, Ord, Show)

instance FromRow Energy where
  fromRow = Energy <$> field

newtype HpSpawn
  = HpSpawn T.Text
  deriving (Eq, FromField, Show)

newtype FirstSpawn
  = FirstSpawn Int
  deriving (Eq, FromField, Ord, Show)

newtype IsBoss
  = IsBoss Bool
  deriving (Eq, FromField, Show)

data FromRowStage
  = FromRowStage Category Level StageName Energy HpSpawn FirstSpawn IsBoss
  deriving (Show)

instance FromRow FromRowStage where
  fromRow = FromRowStage <$> field <*> field <*> field <*> field <*> field <*> field <*> field

newtype EnemyCode
  = EnemyCode Int
  deriving (Eq, Show)

newtype Level
  = Level T.Text
  deriving (Eq, FromField, IsString, Ord, Show, ToField)

instance FromRow Level where
  fromRow = Level <$> field

newtype Target
  = Target T.Text
  deriving (Eq, Show)

newtype Category
  = Category T.Text
  deriving (Eq, FromField, IsString, Ord, Show, ToField)

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
  deriving (Eq, Ord, Show)

getEnergy :: NonEmpty Stage -> Energy
getEnergy = foldr (\(Stage _ _ _ e) -> (e +)) 0

getEnergy' :: NonEmpty StageWithEnemy -> Energy
getEnergy' stages = getEnergy (fst <$> stages)

type StageWithEnemy = (Stage, NonEmpty Enemy)

newtype MinEnergyStages
  = MinEnergyStages (NonEmpty StageWithEnemy)
  deriving (Show)

instance Eq MinEnergyStages where
  MinEnergyStages stagesA == MinEnergyStages stagesB =
      getEnergy' stagesA == getEnergy' stagesB

instance Ord MinEnergyStages where
  MinEnergyStages stagesA <= MinEnergyStages stagesB =
      getEnergy' stagesA <= getEnergy' stagesB
