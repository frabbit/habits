module Veins.Test.AppTH where

import Prelude

import qualified Habits.AppT as AppT

import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import qualified Veins.Data.Has as Has
import Language.Haskell.TH (Dec, Q, mkName)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TS


mkConT :: String -> TS.Type
mkConT = TH.ConT . mkName

mkVarT :: String -> TS.Type
mkVarT = TH.VarT . mkName

typeMonad :: TS.Type
typeMonad = TH.ConT ''Monad

typeFunctor :: TS.Type
typeFunctor = TH.ConT ''Functor

typeApplicative :: TS.Type
typeApplicative = TH.ConT ''Applicative

typeMonadIO :: TS.Type
typeMonadIO = TH.ConT ''MonadIO

typeMonadFail :: TS.Type
typeMonadFail = TH.ConT ''MonadFail

mkTypeMonadReader :: TS.Type -> TS.Type
mkTypeMonadReader = TH.AppT (TH.ConT ''MonadReader)


data Context = Context {
  nameAppEnv :: TH.Name,
  nameApp :: TH.Name,
  nameUnApp :: TH.Name,
  envName :: TH.Name
}



noBang :: TS.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness


typeIO :: TS.Type
typeIO = TH.ConT (mkName "IO")

typeConIO :: TS.Type -> TS.Type
typeConIO = TH.AppT typeIO

-- newtype App a = App {unApp :: AppT (AppEnv App) IO a} deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppEnv App))
mkApp :: Context -> Q [Dec]
mkApp Context { nameApp, nameAppEnv, nameUnApp } = do
  let typeApp = TH.ConT nameApp
  let nameAppT = ''AppT.AppT
  let typeAppT = TH.ConT nameAppT
  let nameA = mkName "a"
  let typeAppEnv = TH.ConT nameAppEnv

  let typeEnv = TH.AppT typeAppEnv typeApp
  let typeUnwrapped = typeAppT `TH.AppT` typeEnv `TH.AppT` typeIO `TH.AppT` TH.VarT nameA

  let con = TH.RecC nameApp [(nameUnApp, noBang, typeUnwrapped)]
  let deriveClauses = [TS.DerivClause Nothing [typeFunctor, typeApplicative, typeMonad, typeMonadIO, mkTypeMonadReader typeEnv, typeMonadFail]]
  let decs = [TH.NewtypeD [] nameApp [TH.PlainTV nameA ()] Nothing con deriveClauses]
  pure decs

-- newtype AppEnv m = AppEnv (Env m)

mkAppEnv :: Context -> Q [Dec]
mkAppEnv Context { nameAppEnv, envName } = pure decs
  where
    nameM = mkName "m"
    nameEnv = envName
    typeEnv = TH.ConT nameEnv
    con = TH.NormalC nameAppEnv [(noBang, TH.AppT typeEnv (TH.VarT nameM))]
    decs = [TH.NewtypeD [] nameAppEnv [TH.PlainTV nameM ()] Nothing con []]

-- runApp :: Env App -> App a -> IO a
-- runApp env a = runAppT' (AppEnv env) (unApp a)

mkRunApp :: Context -> String -> Q [Dec]
mkRunApp Context { nameApp, nameAppEnv, nameUnApp, envName } name = do

  name_env <- TH.newName "env"
  name_a <- TH.newName "a"
  let nameFun = mkName name
  let typeEnvApp = TH.AppT (TH.ConT envName) (TH.ConT nameApp)
  let typeA = TH.VarT name_a
  let typeIOA = typeConIO typeA
  let typeAppA = TH.AppT (TH.ConT nameApp) typeA
  let signature = TH.SigD nameFun (TH.ArrowT `TH.AppT` typeEnvApp `TH.AppT` (TH.ArrowT `TH.AppT` typeAppA `TH.AppT` typeIOA))
  let appEnv_env = TH.AppE (TH.ConE nameAppEnv) (TH.VarE name_env)
  let unApp_a = TH.AppE (TH.VarE nameUnApp) (TH.VarE name_a)
  let runAppT' = TH.VarE 'AppT.runAppT'
  let expr = runAppT' `TH.AppE` appEnv_env `TH.AppE` unApp_a
  let fun = TH.FunD nameFun [TH.Clause [TH.VarP name_env, TH.VarP name_a] (TH.NormalB expr) []]

  let decs = [signature] <> [fun]
  pure decs

-- instance Has.Has y (Env m) => Has.Has y (AppEnv m) where get (AppEnv e) = get e

mkHasInstance :: Context -> Q [Dec]
mkHasInstance Context { nameAppEnv, envName} = do
  let hasName = ''Has.Has
  let getName = 'Has.get

  let constraint = TH.ConT hasName `TH.AppT` mkVarT "y" `TH.AppT` (TH.ConT envName `TH.AppT` mkVarT "m")

  let type' = TH.ConT hasName `TH.AppT` mkVarT "y" `TH.AppT` (TH.ConT nameAppEnv `TH.AppT` mkVarT "m")

  let body = TH.AppE (TH.VarE getName) (TH.VarE $ mkName "e")
  let dec = TH.FunD getName [TH.Clause [TH.ConP nameAppEnv [TH.VarP (mkName "e")]] (TH.NormalB body) []]

  let inst = TH.InstanceD Nothing [constraint] type' [dec]
  pure [inst]


mkBoilerplate :: String -> TH.Name -> Q [Dec]
mkBoilerplate name envName = do
  let nameApp = TH.mkName $ "App_" <> name
  let nameUnApp = TH.mkName $ "unApp_" <> name
  let nameAppEnv = TH.mkName $ "AppEnv_" <> name
  let ctx = Context {
    nameApp,
    nameUnApp,
    nameAppEnv,
    envName
  }
  a <- mkHasInstance ctx
  b <- mkRunApp ctx name
  c <- mkAppEnv ctx
  d <- mkApp ctx
  pure $ a <> b <> c <> d
