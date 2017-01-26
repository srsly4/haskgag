{-# LANGUAGE OverloadedStrings #-}
module Model
    ( openConnection,
      createTable,
      deleteRow,
      closeConnection,
      getGags,
      addRow,
      getGagAtId,
      getLastInsertRowId,
      SQLGag (SQLGag)
    ) where

import           Data.Char
import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

-- |Type to extract data from database
data SQLGag = SQLGag Int T.Text T.Text
    deriving (Show)


-- |Converts SQL tuple to SQLGag
instance FromRow SQLGag where
    fromRow = SQLGag <$> field <*> field <*> field

-- |Converts SQLGag to SQL tuple
instance ToRow SQLGag where
    toRow (SQLGag id_ description path) = toRow (id_, description, path)

-- |Compares all the attributes
instance Eq SQLGag where
    (==) (SQLGag a1 b1 c1) (SQLGag a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

-- |Out database name
databaseName = "haskgagDatabase"

-- |Returns all the SQLGags from database
getGags :: Connection -> IO [SQLGag]
getGags conn = do
    fields <- query_ conn "SELECT id, description, path FROM Gags ORDER BY insertDate DESC" :: IO [SQLGag]
    return fields

-- |Returns single SQLGag from database with given id
getGagAtId :: Connection -> Int -> IO SQLGag
getGagAtId conn id = do
    fields <- query conn "SELECT id, description, path FROM Gags WHERE id = ?" $ Only id :: IO [SQLGag]
    return $ head fields

-- |Deletes row with given id
deleteRow :: Connection -> Int -> IO ()
deleteRow conn id = do
    execute conn "DELETE FROM Gags WHERE id = ?" $ Only id

-- |Adds row to database with given description and path
addRow :: Connection -> String -> String -> IO ()
addRow conn description path = do
    execute conn "INSERT INTO Gags (description, path, insertDate) VALUES (?, ?, DATETIME('now'))" $ (description, path)

-- |Drops table 'Gags'
dropTable :: Connection -> IO ()
dropTable conn = do
    execute_ conn "DROP TABLE IF EXISTS Gags"

-- |Deletes all content of 'Gags' table
clearTable :: Connection -> IO ()
clearTable conn = do
    execute_ conn "DELETE FROM Gags"

-- |Opens connection with database
openConnection :: IO Connection
openConnection = open $ databaseName ++ ".db"

-- |Creates table 'Gags'
createTable :: Connection -> IO ()
createTable conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS Gags (id INTEGER PRIMARY KEY AUTOINCREMENT, description TEXT, path TEXT, insertDate DATETIME)"

-- |Closes open connection
closeConnection :: Connection -> IO ()
closeConnection conn = do
    close conn

-- |Returns the last inserted to database row id
getLastInsertRowId :: Connection -> IO Int
getLastInsertRowId conn = do
    ret <- lastInsertRowId conn
    return $ fromIntegral ret