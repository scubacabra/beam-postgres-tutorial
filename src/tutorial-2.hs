{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Tutorial2 where

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

deriving instance Show User

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail

data AddressT f = Address
  { _addressId :: C f (Auto Int)
  , _addressLine1 :: C f Text
  , _addressLine2 :: C f (Maybe Text)
  , _addressCity :: C f Text
  , _addressState :: C f Text
  , _addressZip :: C f Text
  , _addressForUser :: PrimaryKey UserT f
  } deriving (Generic)

type Address = AddressT Identity
type AddressId = PrimaryKey AddressT Identity

deriving instance Show UserId
deriving instance Show Address

instance Beamable AddressT
instance Beamable (PrimaryKey AddressT)

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f (Auto Int)) deriving Generic
    primaryKey = AddressId . _addressId

data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT)
  , _shoppingCartUserAddresses :: f (TableEntity AddressT)
  } deriving (Generic)

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) = tableLenses

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) = tableLenses

ShoppingCartDb (TableLens shoppingCartUsers)
               (TableLens shoppingCartUserAddresses) = dbLenses

allUsers :: Q PgSelectSyntax ShoppingCartDb s (UserT (QExpr PgExpressionSyntax s))
allUsers = all_ (_shoppingCartUsers shoppingCartDb)

insertUsers :: Connection -> IO ()
insertUsers conn =
  withDatabaseDebug putStrLn conn $ B.runInsert $
    B.insert (_shoppingCartUsers shoppingCartDb) $
    insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                 , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                 , User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                 , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                 , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                 , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                 , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                 , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                 ]

selectAllUsers :: Connection -> IO ()
selectAllUsers conn =
  withDatabaseDebug putStrLn conn $ do
    users <- runSelectReturningList $ select allUsers
    mapM_ (liftIO . putStrLn . show) users

sortUsersByFirstName :: Connection -> IO ()
sortUsersByFirstName conn =
  withDatabaseDebug putStrLn conn $ do
    users <- runSelectReturningList $ select sortUsersByFirstName
    mapM_ (liftIO . putStrLn . show) users
  where
    sortUsersByFirstName = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) allUsers

boundedUsers :: Connection -> IO ()
boundedUsers conn =
  withDatabaseDebug putStrLn conn $ do
    users <- runSelectReturningList $ select boundedQuery
    mapM_ (liftIO . putStrLn . show) users
  where
    boundedQuery = limit_ 1 $ offset_ 1 $ orderBy_ (asc_ . _userFirstName) $ allUsers

userCount :: Connection -> IO ()
userCount conn =
  withDatabaseDebug putStrLn conn $ do
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")
  where
    userCount = aggregate_ (\u -> as_ @Int countAll_) allUsers

numberOfUsersByName :: Connection -> IO ()
numberOfUsersByName conn =
  withDatabaseDebug putStrLn conn $ do
    countedByName <- runSelectReturningList $ select numberOfUsersByName
    mapM_ (liftIO . putStrLn . show) countedByName
  where
    numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) allUsers

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=shoppingcart2"
  return ()
