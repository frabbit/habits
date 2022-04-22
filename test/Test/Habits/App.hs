module Test.Habits.App where


import           Control.Monad.Reader.Class     ( MonadReader )

import           Control.Monad.IO.Class         ( MonadIO )


import           Control.Monad.Except           ( ExceptT(ExceptT)
                                                , runExceptT
                                                )
import           Data.Variant                   ( Variant )
import           Habits.AppT                    ( AppT(..)
                                                , runAppT
                                                , runAppT'
                                                )
import           Test.Habits.AppEnv             ( AppEnv )

newtype App a = App { unApp :: AppT (AppEnv App) IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppEnv App))

runApp :: forall a . AppEnv App -> ExceptT (Variant '[]) App a -> IO a
runApp env app = runAppT env (ExceptT . unApp . runExceptT $ app)

runApp' :: forall a . AppEnv App -> App a -> IO a
runApp' env app = runAppT' env (unApp app)

