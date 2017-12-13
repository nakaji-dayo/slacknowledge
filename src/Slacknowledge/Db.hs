{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Slacknowledge.Db where

import           Data.Int                    (Int32)
import           Database.HDBC.MySQL
import           Database.HDBC.Query.TH      (defineTableFromDB')
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.HDBC.Schema.MySQL  (driverMySQL)
-- import           Database.Record.TH          (derivingShow)
import           Database.Relational.Query
import           Language.Haskell.TH         (Dec, Q, TypeQ)
import GHC.Generics
import Database.HDBC.Record (runQuery, runInsert)
import Database.HDBC (IConnection, SqlValue, commit)

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")


world :: Relation () (Int32, String)
world = relation $ return (value 0 >< value "World")


helloWorld :: Relation () (Int32, (String, String))
helloWorld = relation $ do
    h <- query hello
    w <- query world
    on $ h ! fst' .=. w ! fst'
    return $ h ! fst' >< (h ! snd' >< w ! snd')


connect :: IO Connection
connect = connectMySQL defaultMySQLConnectInfo
  { mysqlUser     = "slacknowledge"
  , mysqlPassword = "slacknowledge"
  , mysqlDatabase = "slacknowledge"
  , mysqlHost     = "127.0.0.1"
  }

convTypes :: [(String, TypeQ)]
convTypes = [("MEDIUMINT", [t|Int32|])]


defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB'
    connect
    (defaultConfig { normalizedTableName = False })
    (driverMySQL { typeMap = convTypes })
    "slacknowledge"
    tableName
    [''Show, ''Generic]

runInsert' insert = do
  conn <- connect
  runInsert conn insert ()
  commit conn

-- runTeam1 = do
--   conn <- connect
--   putStrLn $ "SQL: " ++ show team1
--   records <- runQuery conn (relationalQuery team1) ()
--   mapM_ print records
--   putStrLn ""
