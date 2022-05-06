module Habits.TH.TypeOf where

import           Control.Monad                  ( (<=<) )

import           Language.Haskell.TH            ( Exp
                                                , Name
                                                , Q
                                                , pprint
                                                , reify, Dec
                                                )
import           Language.Haskell.TH.Syntax     ( Lift(lift) )
import Control.Monad.IO.Class (liftIO)

getStaticType :: Name -> Q Exp
getStaticType name = do

  expr <- lift <=< fmap pprint . reify $ name
  info <- reify name
  let str = pprint info
  liftIO $ putStrLn str
  liftIO $ putStrLn "har"
  pure expr

getStaticDecl :: Name -> Q [Dec]
getStaticDecl name = do
  info <- reify name
  let str = pprint info
  liftIO $ putStrLn str
  liftIO $ putStrLn "har"
  pure []
