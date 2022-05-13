module Veins.Test.AppTH where
import Language.Haskell.TH (Q, Dec)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TS
import qualified Language.Haskell.TH.Lib as TL
import Control.Monad.IO.Class (liftIO)



typeMonad :: TS.Type
typeMonad = TH.ConT $ TH.mkName "Monad"

typeFunctor :: TS.Type
typeFunctor = TH.ConT $ TH.mkName "Functor"

typeApplicative :: TS.Type
typeApplicative = TH.ConT $ TH.mkName "Applicative"

typeMonadIO :: TS.Type
typeMonadIO = TH.ConT $ TH.mkName "MonadIO"

mkTypeMonadReader :: TS.Type -> TS.Type
mkTypeMonadReader = TH.AppT (TH.ConT (TH.mkName "MonadReader"))

nameAppEnv :: TS.Name
nameAppEnv = TH.mkName "AppEnv"

noBang :: TS.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

nameApp :: TS.Name
nameApp = TH.mkName "App"

-- newtype App a = App {unApp :: AppT (AppEnv App) IO a} deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppEnv App))
mkApp :: Q [Dec]
mkApp = do
  let typeApp = TH.ConT nameApp
  let nameAppT = TH.mkName "AppT"
  let nameA = TH.mkName "a"
  let nameUnApp = TH.mkName "unApp"
  let nameIO = TH.mkName "IO"
  let envType = TH.AppT (TH.ConT nameAppEnv) typeApp
  let appType =  ((TH.ConT nameAppT `TH.AppT` envType) `TH.AppT` TH.ConT nameIO) `TH.AppT` TH.VarT nameA
  let con = TH.RecC nameApp [(nameUnApp, noBang, appType )]
  let deriveClauses = [ TS.DerivClause Nothing [typeFunctor, typeApplicative, typeMonad, typeMonadIO, mkTypeMonadReader envType] ]
  let decs = [TH.NewtypeD [] nameApp [TH.PlainTV nameA ()] Nothing con deriveClauses]
  liftIO $ print decs
  pure decs


-- newtype AppEnv m = AppEnv (Env m)

mkAppEnv :: Q [Dec]
mkAppEnv = do
  let nameM = TH.mkName "m"
  let nameEnv = TH.mkName "Env"
  let con = TH.NormalC nameAppEnv [(noBang, TH.AppT (TH.ConT nameEnv) (TH.VarT nameM))]
  let decs = [TH.NewtypeD [] nameAppEnv [TH.PlainTV nameM ()] Nothing con []]
  pure decs
