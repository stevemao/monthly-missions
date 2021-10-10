{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BattleCats.MonthlyMissions.Types where

import           Data.List.NonEmpty
import qualified Data.Text                        as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           GHC.Exts                         (IsString (..))

newtype StageName = StageName T.Text
    deriving (Show, FromField, Eq, Ord)

instance FromRow StageName where
  fromRow = StageName <$> field

newtype Energy = Energy Int
    deriving (Show, FromField, Eq, Num, Ord)

instance FromRow Energy where
  fromRow = Energy <$> field

newtype HpSpawn = HpSpawn T.Text
  deriving (Show, FromField, Eq)

newtype FirstSpawn = FirstSpawn Int
  deriving (Show, FromField, Eq, Ord)

data FromRowStage = FromRowStage Level StageName Energy HpSpawn FirstSpawn
    deriving (Show)

instance FromRow FromRowStage where
  fromRow = FromRowStage <$> field <*> field <*> field <*> field <*> field

newtype EnemyCode = EnemyCode Int

newtype Level = Level T.Text
    deriving (Show, FromField, Eq, IsString, ToField, Ord)

instance FromRow Level where
  fromRow = Level <$> field

newtype Target = Target T.Text
    deriving (Show, Eq)

newtype Category = Category T.Text
    deriving (Show, IsString, ToField)

data Location = LocationLevel Level | LocationCategory Category
    deriving Show

data Mission = Mission Location Target
    deriving Show

newtype EnemyUnitsTSV = EnemyUnitsTSV T.Text

data Enemy = Enemy HpSpawn FirstSpawn Target
  deriving (Show, Eq)

newtype FastestEnemy = FastestEnemy Enemy

instance Eq FastestEnemy where
  FastestEnemy (Enemy _ firstSpawnA _) == FastestEnemy (Enemy _ firstSpawnB _) = firstSpawnA == firstSpawnB

instance Ord FastestEnemy where
  FastestEnemy (Enemy _ firstSpawnA _) <= FastestEnemy (Enemy _ firstSpawnB _) = firstSpawnA <= firstSpawnB

data Stage = Stage Level StageName Energy
  deriving (Show, Eq, Ord)

getEnergy :: NonEmpty Stage -> Energy
getEnergy = foldr (\(Stage _ _ e) -> (e +)) 0

getEnergy' :: NonEmpty StageWithEnemy -> Energy
getEnergy' stages = getEnergy (fst <$> stages)

type StageWithEnemy = (Stage, NonEmpty Enemy)

newtype MinEnergyStages = MinEnergyStages (NonEmpty StageWithEnemy)
  deriving (Show)

instance Eq MinEnergyStages where
  MinEnergyStages stagesA == MinEnergyStages stagesB = getEnergy' stagesA == getEnergy' stagesB

instance Ord MinEnergyStages where
  MinEnergyStages stagesA <= MinEnergyStages stagesB = getEnergy' stagesA <= getEnergy' stagesB
