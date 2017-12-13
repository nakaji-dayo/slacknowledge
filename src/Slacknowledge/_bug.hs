{-# LANGUAGE TemplateHaskell #-}

module Slacknowledge.Db where

import           Data.Int                    (Int32)
import           Database.HDBC.Query.TH      (defineTableFromDB')
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.Relational.Query
import           Language.Haskell.TH         (Dec, Q, TypeQ)

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

defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB'
    connect
    (defaultConfig { normalizedTableName = False })
    (driverMySQL { typeMap = convTypes })
    "slacknowledge"
    tableName
    [derivingShow]

$(defineTable "teams")
