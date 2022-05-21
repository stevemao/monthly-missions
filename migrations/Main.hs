{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Database.SQLite.Simple

main :: IO ()
main = do
  conn <- open "./data/stages.db"

  execute_ conn "ALTER TABLE stages ADD schedule TEXT;"

  execute_ conn "UPDATE stages SET schedule = 'Mon' WHERE level = 'Crimson Catastrophe' OR level = 'Crimson Vengeance';"
  execute_ conn "UPDATE stages SET schedule = 'Tue' WHERE level = 'The Rolling Dead' OR level = 'The Dead Keep Rolling';"
  execute_ conn "UPDATE stages SET schedule = 'Tue' WHERE level = 'Red Sky at Morning' OR level = 'Red Sky at Night';"
  execute_ conn "UPDATE stages SET schedule = 'Wed' WHERE level = 'Heaven of Darkness' OR level = 'NEO Darkness Heaven';"
  execute_ conn "UPDATE stages SET schedule = 'Thu' WHERE level = 'Dimension of Despair' OR level = 'Dimension of Delirium';"
  execute_ conn "UPDATE stages SET schedule = 'Fri' WHERE level = 'Peerless' OR level = 'Peerless Too';"
  execute_ conn "UPDATE stages SET schedule = 'Fri' WHERE level = 'The Great Diablo';"
  execute_ conn "UPDATE stages SET schedule = 'Sat' WHERE level = 'Wrath of Heaven' OR level = 'Wrath of the Divine';"
  execute_ conn "UPDATE stages SET schedule = 'Sat' WHERE level = 'Typhoon Nemo' OR level = 'Typhoon Nihil';"
  execute_ conn "UPDATE stages SET schedule = 'Sun' WHERE level = 'Sweet Irony' OR level = 'Bitter Irony';"
  execute_ conn "UPDATE stages SET schedule = 'Sun' WHERE level = 'The 2nd Dimension' OR level = 'The 3rd Dimension';"

  close conn
