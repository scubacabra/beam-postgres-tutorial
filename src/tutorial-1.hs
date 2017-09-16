{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Tutorial1 where

import           Data.Text                  (Text)
import           Database.Beam              as B
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

data UserT f = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  } deriving (Generic)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

allUsers :: Q PgSelectSyntax ShoppingCartDb s (UserT (QExpr PgExpressionSyntax s))
allUsers = all_ (_shoppingCartUsers shoppingCartDb)

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=shoppingcart1"
  return ()
