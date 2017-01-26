module Main where

import Control.Applicative ((<$>), optional)
import Happstack.Lite
import Control.Monad
import Controller
import Model
import Templates
import System.Directory (doesDirectoryExist, createDirectory)

main :: IO ()
main = do
    conn <- openConnection
    createTable conn
    --create tmp folder
    tmpExists <- doesDirectoryExist "tmp"
    when (not tmpExists) (createDirectory "tmp")
    let config = Just ServerConfig {port = 8888, ramQuota=32000000, diskQuota=32000000, tmpDir = "./tmp/"}
    serve config entrance
