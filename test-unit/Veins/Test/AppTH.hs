{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Veins.Test.AppTH where

import Control.Monad.IO.Class (liftIO)
import Language.Haskell.Meta (parseDecs)
import Language.Haskell.TH (Dec, Q, mkName)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TS
import Habits.Domain.Account (Account(_name))

mkConT :: String -> TS.Type
mkConT = TH.ConT . mkName

mkVarT :: String -> TS.Type
mkVarT = TH.VarT . mkName

typeMonad :: TS.Type
typeMonad = mkConT "Monad"

typeFunctor :: TS.Type
typeFunctor = mkConT "Functor"

typeApplicative :: TS.Type
typeApplicative = mkConT "Applicative"

typeMonadIO :: TS.Type
typeMonadIO = mkConT "MonadIO"

mkTypeMonadReader :: TS.Type -> TS.Type
mkTypeMonadReader = TH.AppT (mkConT "MonadReader")


data Context = Context {
  _nameAppEnv :: TH.Name,
  _nameApp :: TH.Name,
  _nameUnApp :: TH.Name,
  _envName :: TH.Name
}



noBang :: TS.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness


typeIO :: TS.Type
typeIO = TH.ConT (mkName "IO")

typeConIO :: TS.Type -> TS.Type
typeConIO = TH.AppT typeIO

-- newtype App a = App {unApp :: AppT (AppEnv App) IO a} deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppEnv App))
mkApp :: Context -> Q [Dec]
mkApp Context { _nameApp, _nameAppEnv, _nameUnApp } = do
  let nameApp = _nameApp
  let typeApp = TH.ConT nameApp
  let nameAppT = mkName "AppT"
  let typeAppT = TH.ConT nameAppT
  let nameA = mkName "a"
  let nameUnApp = _nameUnApp
  let nameAppEnv = _nameAppEnv
  let typeAppEnv = TH.ConT nameAppEnv

  let typeEnv = TH.AppT typeAppEnv typeApp
  let typeUnwrapped = typeAppT `TH.AppT` typeEnv `TH.AppT` typeIO `TH.AppT` TH.VarT nameA

  let con = TH.RecC nameApp [(nameUnApp, noBang, typeUnwrapped)]
  let deriveClauses = [TS.DerivClause Nothing [typeFunctor, typeApplicative, typeMonad, typeMonadIO, mkTypeMonadReader typeEnv]]
  let decs = [TH.NewtypeD [] nameApp [TH.PlainTV nameA ()] Nothing con deriveClauses]
  liftIO $ print decs
  pure decs

-- newtype AppEnv m = AppEnv (Env m)

mkAppEnv :: Context -> Q [Dec]
mkAppEnv Context { _nameApp, _nameAppEnv, _envName } = pure decs
  where
    nameAppEnv = _nameAppEnv
    nameM = mkName "m"
    nameEnv = _envName
    typeEnv = TH.ConT nameEnv
    con = TH.NormalC nameAppEnv [(noBang, TH.AppT typeEnv (TH.VarT nameM))]
    decs = [TH.NewtypeD [] nameAppEnv [TH.PlainTV nameM ()] Nothing con []]

-- runApp :: Env App -> App a -> IO a
-- runApp env a = runAppT' (AppEnv env) (unApp a)

mkRunApp :: Context -> String -> Q [Dec]
mkRunApp Context { _nameApp, _nameAppEnv, _nameUnApp, _envName } name = do

  name_env <- TH.newName "env"
  name_a <- TH.newName "a"
  let nameApp = _nameApp
  let nameFun = mkName name
  let nameEnv = _envName
  let typeEnvApp = TH.AppT (TH.ConT nameEnv) (TH.ConT nameApp)
  let typeA = TH.VarT name_a
  let typeIOA = typeConIO typeA
  let typeAppA = TH.AppT (TH.ConT nameApp) typeA
  let signature = TH.SigD nameFun (TH.ArrowT `TH.AppT` typeEnvApp `TH.AppT` (TH.ArrowT `TH.AppT` typeAppA `TH.AppT` typeIOA))
  let appEnv_env = TH.AppE (TH.ConE _nameAppEnv) (TH.VarE name_env)
  let unApp_a = TH.AppE (TH.VarE _nameUnApp) (TH.VarE name_a)
  let runAppT' = TH.VarE (mkName "runAppT'")
  let expr = runAppT' `TH.AppE` appEnv_env `TH.AppE` unApp_a
  let fun = TH.FunD nameFun [TH.Clause [TH.VarP name_env, TH.VarP name_a] (TH.NormalB expr) []]

  let decs = [signature] <> [fun]
  pure decs

-- instance Has.Has y (Env m) => Has.Has y (AppEnv m) where get (AppEnv e) = get e

mkHasInstance :: Context -> Q [Dec]
mkHasInstance Context { _nameApp, _nameAppEnv, _envName} = do
  let constraint = mkConT "Has.Has" `TH.AppT` mkVarT "y" `TH.AppT` (TH.ConT _envName `TH.AppT` mkVarT "m")

  let type' = mkConT "Has.Has" `TH.AppT` mkVarT "y" `TH.AppT` (TH.ConT _nameAppEnv `TH.AppT` mkVarT "m")

  let body = TH.AppE (TH.VarE $ mkName "get") (TH.VarE $ mkName "e")
  let dec = TH.FunD (mkName "get") [TH.Clause [TH.ConP _nameAppEnv [TH.VarP (mkName "e")]] (TH.NormalB body) []]

  let inst = TH.InstanceD Nothing [constraint] type' [dec]
  pure [inst]


mkBoilerplate :: String -> TH.Name -> Q [Dec]
mkBoilerplate name _envName = do
  let _nameApp = TH.mkName $ "App_" <> name
  let _nameUnApp = TH.mkName $ "unApp_" <> name
  let _nameAppEnv = TH.mkName $ "AppEnv_" <> name
  let ctx = Context {
    _nameApp,
    _nameUnApp,
    _nameAppEnv,
    _envName
  }
  a <- mkHasInstance ctx
  b <- mkRunApp ctx name
  c <- mkAppEnv ctx
  d <- mkApp ctx
  liftIO $ print (a <> b <> c <> d)
  pure $ a <> b <> c <> d
