module Slacknowledge.Relation where

import qualified Slacknowledge.Team as Team
import Slacknowledge.Db
import Database.Relational.Query

-- team1 = relation $ do
--   t <- query Team.team
--   wheres $ t ! Team.id' .=. value 6
--   return t

insertTeam teamId teamName accessToken botAccessToken = derivedInsertValue $ do
  Team.teamId' <-# value teamId
  Team.teamName' <-# value teamName
  Team.accessToken' <-# value accessToken
  Team.botAccessToken' <-# value botAccessToken
  return unitPlaceHolder
