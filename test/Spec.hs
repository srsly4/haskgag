{-# LANGUAGE TemplateHaskell #-}
import           Test.QuickCheck
import           Test.HUnit
import           Model
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.HUnit
import           ModelObjects
import           Controller

-- |Tests conversion from SQLGag to Gag
prop_testParse :: Int -> String -> String -> Bool
prop_testParse iden title image = (==)
    MkGag { ModelObjects.id=iden, title = (T.pack title), image = (T.pack $ "/static/gags/" ++ image)}
    (sqlGagToGag (SQLGag iden (T.pack title) (T.pack image)))

return []


main :: IO (Counts, Bool)
main = do
    hUnit <-runTestTT tests
    quickChecks <- $quickCheckAll
    return (hUnit, quickChecks)


tests = TestList [TestLabel "connection test" testConn, TestLabel "parse test" testParse]

-- |Tests database functions: inserting, deleting and extracting data
testConn = TestCase (
    do
    conn <- openConnection
    extr1 <- getGags conn

    addRow conn "t1" "t2"
    rowId <- getLastInsertRowId conn
    extr <- getGagAtId conn $ fromIntegral rowId
    assertEqual "test extracted value" (SQLGag (fromIntegral rowId) (T.pack "t1") (T.pack "t2")) extr
    deleteRow conn $ fromIntegral rowId

    extr2 <- getGags conn
    assertEqual "test remaining rows" extr1 extr2
    closeConnection conn)

-- |Tests conversion from SQLGag to Gag
testParse = TestCase (assertEqual "parse test"
    MkGag { ModelObjects.id=100, title = (T.pack "title"), image = (T.pack "./static/gags/image")}
    (sqlGagToGag (SQLGag 100 (T.pack "title") (T.pack "image"))))
