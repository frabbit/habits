{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Habits.UseCases.Register.Live where
import           Habits.Domain.AccountNew       ( AccountNew
                                                  ( AccountNew
                                                  , email
                                                  , name
                                                  )
                                                )
import           Habits.Domain.AccountRepo      ( AddError(AddError) )
import           Habits.Domain.AccountRepo.Class
                                                ( AccountRepo
                                                , add
                                                )
import           Habits.Domain.Email            ( Email(Email) )
import           Habits.UseCases.Register       ( RegisterError(RegisterError)
                                                , RegisterExec
                                                , RegisterResponse
                                                  ( RegisterResponse
                                                  )
                                                )
import           Haskus.Utils.Variant.Excepts   ( catchE
                                                , throwE
                                                , catchAllE
                                                )


register :: (Monad m, AccountRepo m) => RegisterExec m
register req = do
  catchAllE (\e -> throwE RegisterError) (add (AccountNew { email = Email "what@do.de", name = "aha" }))
  pure $ RegisterResponse True

