module Habits.UseCases.Register.Live where
import           Data.Variant                   ( catchM
                                                , throwM
                                                )
import           Habits.Domain.AccountNew       ( AccountNew
                                                  ( AccountNew
                                                  , email
                                                  , name
                                                  )
                                                )
import           Habits.Domain.AccountRepo      ( AddError )
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


register :: (Monad m, AccountRepo m) => RegisterExec m
register _ = do
  catchM (add (AccountNew { email = Email "what@do.de", name = "aha" }))
         (\(_ :: AddError) -> throwM RegisterError)
  pure $ RegisterResponse True

