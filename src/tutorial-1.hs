{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Tutorial1 where

import           Data.Text                  (Text)
import           Database.Beam              as B
import           Database.PostgreSQL.Simple

data UserT f = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  } deriving (Generic)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=shoppingcart1"
  return ()
