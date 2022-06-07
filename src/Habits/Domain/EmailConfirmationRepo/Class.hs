module Habits.Domain.EmailConfirmationRepo.Class where

import Habits.Prelude
import Veins.Data.Has (Has)
import Habits.Domain.EmailConfirmationRepo (EmailConfirmationRepo, GetById, Add, GetByNonce, GetByNonceOrFail)
import qualified Habits.Domain.EmailConfirmationRepo as EmailConfirmationRepo
import Habits.Utils (applyFirstM)

class EmailConfirmationRepoM m where
  getById :: GetById m
  getByNonce :: GetByNonce m
  add :: Add m
  getByNonceOrFail :: GetByNonceOrFail m


instance (MonadReader env m, Has (EmailConfirmationRepo m) env) => EmailConfirmationRepoM m where
  getById = applyFirstM EmailConfirmationRepo.getById
  getByNonce = applyFirstM EmailConfirmationRepo.getByNonce
  add = applyFirstM EmailConfirmationRepo.add
  getByNonceOrFail = applyFirstM EmailConfirmationRepo.getByNonceOrFail
