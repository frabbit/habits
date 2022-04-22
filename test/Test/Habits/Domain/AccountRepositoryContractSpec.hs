{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Habits.Domain.AccountRepositoryContractSpec where
import           Habits.Domain.AccountRepo.Class
                                                ( AccountRepo )
import qualified Habits.Domain.AccountRepo.Class
                                               as ARC

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Lens                   ( (^.) )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Data                      ( Proxy )
import           Data.Function                  ( (&) )
import qualified Data.Text                     as Text
import           Data.Variant                   ( Catch
                                                , CouldBe
                                                , Variant
                                                , catchM
                                                )
import           GHC.Stack                      ( HasCallStack )
import           Habits.AppT                    ( eliminate )
import           Habits.Domain.Account          ( name
                                                )
import           Habits.Domain.AccountId
import qualified Habits.Domain.AccountNew      as AN
import           Habits.Domain.AccountRepo      ( AddError
                                                , RepositoryError
                                                )
import           Habits.Domain.Email
import           Test.Habits.App                ( runApp
                                                , runApp'
                                                )
import           Test.Habits.AppEnv             ( mkAppEnv )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           UnliftIO                       ( MonadIO )

toThrow
  :: forall x e m a e'
   . (Catch x e e', MonadIO m, Exception x)
  => ExceptT (Variant e) m a
  -> ExceptT (Variant e') m a
toThrow x = x `catchM` (\(y :: x) -> throw y)


mkSpec
  :: (HasCallStack, Monad m, MonadIO m, AccountRepo m)
  => (forall a . m a -> IO a)
  -> Spec
mkSpec unlift = describe "AccountRepositoryContract" $ do
  it "should work" $ unlift $ eliminate $ do
    let account =
          AN.AccountNew { AN._email = Email "abc@hello.de", AN._name = "what" }
    AccountId accountId <- ARC.add account & toThrow @AddError
    acc <- ARC.getById (AccountId accountId) & toThrow @RepositoryError

    liftIO $ (acc ^. name) `shouldBe` ("what" :: Text.Text)
    --acc.accountId `shouldBe` (AccountId accountId)
    liftIO $ (Text.length accountId > 0) `shouldBe` True

spec :: Spec
spec = mkSpec (runApp' mkAppEnv)
