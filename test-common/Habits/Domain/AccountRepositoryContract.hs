{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Habits.Domain.AccountRepositoryContract where
import           Habits.Domain.AccountRepo.Class
                                                ( AccountRepo )
import qualified Habits.Domain.AccountRepo.Class
                                               as ARC



import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.IO.Class         ( liftIO )

import           Data.Function                  ( (&) )
import qualified Data.Text                     as Text
import           Data.Variant                   ( Variant
                                                , catchM
                                                )

import           GHC.Stack                      ( HasCallStack )
import           Habits.AppT                    ( eliminate )
import qualified Habits.Domain.Account         as A
import qualified Habits.Domain.AccountId       as AccountId
import           Habits.Domain.AccountRepo      ( AccountNotFoundError
                                                  ( AccountNotFoundError
                                                  )
                                                , AddError
                                                , RepositoryError
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                )
import           Test.Hspec.Expectations.Lifted ( expectationFailure
                                                , shouldBe
                                                )
import           UnliftIO                       ( MonadIO )

import           Utils                          ( sampleIO
                                                , toThrow
                                                )

import qualified Data.Functor

import           Habits.Domain.AccountId        ( AccountId )
import           Habits.TH.TypeOf               ( getStaticDecl )

$( getStaticDecl 'Data.Functor.fmap)

toThrowAll' :: _ => ExceptT (Variant e) m a -> ExceptT (Variant '[]) m a
toThrowAll' x =
  x
    & toThrow @AddError
    & toThrow @RepositoryError
    & toThrow @AccountNotFoundError

mkSpec :: (HasCallStack, MonadIO m, AccountRepo m) => (m () -> IO ()) -> Spec
mkSpec unlift = parallel $ describe "AccountRepositoryContract" $ do
  let embed = unlift . eliminate . toThrowAll'
  describe "add should" $ do
    it "persist the account to the repository" $ embed do
      accountNew <- sampleIO
      accountId  <- ARC.add accountNew
      acc        <- ARC.getById accountId
      acc `shouldBe` A.fromAccountNew accountNew accountId

      ((Text.length . AccountId.unwrap $ accountId) > 0) `shouldBe` True
  describe "getById should" $ do
    it "fail with AccountNotFound when repository is empty" $ embed do
      (accountId :: AccountId) <- sampleIO
      res <- catchM (ARC.getById accountId & fmap Right)
                    (\(e :: AccountNotFoundError) -> pure . Left $ e)
      case res of
        Left  e -> e `shouldBe` AccountNotFoundError
        Right _ -> expectationFailure "AccountNotFoundError expected"


      ((Text.length . AccountId.unwrap $ accountId) > 0) `shouldBe` True
