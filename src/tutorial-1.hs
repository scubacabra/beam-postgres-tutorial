{-# LANGUAGE OverloadedStrings #-}
module Tutorial1 where

import           Database.PostgreSQL.Simple

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=shoppingcart1"
  return ()
