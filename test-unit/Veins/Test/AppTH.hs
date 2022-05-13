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

-- runApp :: Env App -> App a -> IO a
-- runApp env a = runAppT' (AppEnv env) (unApp a)

typeIO :: TS.Type
typeIO = TH.ConT (TH.mkName "IO")

typeConIO :: TS.Type -> TS.Type
typeConIO = TH.AppT typeIO

mkRunApp :: Q [Dec]
mkRunApp = do
  let nameFun = TH.mkName "runApp"
  let nameEnv = TH.mkName "Env"
  let nameA = TH.mkName "a"
  let typeEnvApp = TH.AppT (TH.ConT nameEnv) (TH.ConT nameApp)
  let typeA = TH.VarT nameA
  let typeIOA = typeConIO typeA
  let typeAppA = TH.AppT (TH.ConT nameApp) typeA
  let signature = TH.SigD nameFun ( TH.ArrowT `TH.AppT` typeEnvApp `TH.AppT` (TH.ArrowT `TH.AppT` typeAppA `TH.AppT` typeIOA))
  let appEnv_env = TH.AppE (TH.ConE (TH.mkName "AppEnv")) (TH.VarE (TH.mkName "env"))
  let unApp_a = TH.AppE (TH.VarE (TH.mkName "unApp")) (TH.VarE nameA)
  let runAppT' = TH.VarE (TH.mkName "runAppT'")
  let expr = runAppT' `TH.AppE`  appEnv_env `TH.AppE` unApp_a
  let fun = TH.FunD nameFun [TH.Clause [TH.VarP (TH.mkName "env"), TH.VarP nameA] (TH.NormalB expr) []]
  let decs = [signature, fun]
  pure decs

-- instance Has.Has y (Env m) => Has.Has y (AppEnv m) where get (AppEnv e) = get e

mkHasInstance :: Q [Dec]
mkHasInstance = do
  let constraint = TH.ConT (TH.mkName "Has.Has") `TH.AppT` TH.VarT (TH.mkName "y") `TH.AppT` (TH.ConT (TH.mkName "Env") `TH.AppT` TH.VarT (TH.mkName "m"))

  let type' = TH.ConT (TH.mkName "Has.Has") `TH.AppT` TH.VarT (TH.mkName "y") `TH.AppT` (TH.ConT (TH.mkName "AppEnv") `TH.AppT` TH.VarT (TH.mkName "m"))

  let body = TH.AppE (TH.VarE $ TH.mkName "get") (TH.VarE $ TH.mkName "e")
  let dec = TH.FunD (TH.mkName "get") [TH.Clause [TH.ConP (TH.mkName "AppEnv") [TH.VarP (TH.mkName "e")]] (TH.NormalB body) []]

  let inst = TH.InstanceD Nothing [constraint] type' [dec]
  pure [inst]

