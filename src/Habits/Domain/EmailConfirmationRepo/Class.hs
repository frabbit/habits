module Habits.Domain.EmailConfirmationRepo.Class where

import Habits.Prelude
import Veins.Data.Has (Has)
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo, GetById, Add)
import qualified Habits.Domain.EmailConfirmationRepo as EmailConfirmationRepo
import Habits.Utils (applyFirstM)

class EmailConfirmationRepoM m where
  getById :: GetById m
  add :: Add m


instance (MonadReader env m, Has (EmailConfirmationRepo m) env) => EmailConfirmationRepoM m where
  getById = applyFirstM EmailConfirmationRepo.getById
  add = applyFirstM EmailConfirmationRepo.add
