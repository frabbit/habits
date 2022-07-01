module Main where

import Habits.Prelude
import qualified Habits.HabitsApp as HabitsApp
import Habits.Options (parseCommand)

main :: IO ()
main = do
  cmd <- parseCommand
  HabitsApp.main cmd.port