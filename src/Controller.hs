{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Controller
    ( entrance ,
      sqlGagToGag
    ) where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, concat)
import Data.Text.Lazy (unpack)
import Control.Monad
import Happstack.Lite
import System.Directory
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad.IO.Class
import ModelObjects
import Templates
import Model

-- |The routing starting point for server
entrance :: ServerPart Response
entrance = msum [ dir "static" $ staticServer,
                  dir "processadd" $ processAddPage,
                  dir "removegag" $ processRemovePage,
                  dir "add" $ addPage,
                  homePage ]

-- |Provides static conent server like stylesheets, images
staticServer :: ServerPart Response
staticServer = serveDirectory EnableBrowsing ["index.html"] "./static/"

-- |Converts database model type to template type
sqlGagToGag :: SQLGag -> Gag
sqlGagToGag (SQLGag iden ttle pth) = MkGag { ModelObjects.id=iden, title = ttle, image = Data.Text.concat ["/static/gags/", pth] }

-- |Controller for gags list and the homepage
homePage :: ServerPart Response
homePage = do
        conn <- liftIO $ openConnection
        sgags <- liftIO $ getGags conn
        let gags = map sqlGagToGag sgags
        liftIO $ closeConnection conn
        ok $ toResponse (pageTemplate "HaskGag - place of gags" $
            gagListTemplate gags)

-- |Controller for the adding gag form page
addPage :: ServerPart Response
addPage = ok $ toResponse (pageTemplate "HaskGag - adding a gag" gagAddTemplate)


-- |Controller for submited adding gag form. It copies the temporarily file to the statics folder and adds an entry to database
processAddPage :: ServerPart Response
processAddPage = do
    (tmpFile, uploadName, contentType) <- lookFile "image"
    if ctType contentType == "image" then do
        let fname = "static/gags/"++uploadName
        fexist <- liftIO $ doesFileExist fname
        liftIO $ when (not fexist) (copyFile tmpFile (fname))
        gagName <- lookText "title"
        conn <- liftIO $ openConnection
        liftIO $ addRow conn (unpack gagName) uploadName
        ok $ toResponse (pageTemplate "HaskGag – adding a gag" $ H.toHtml (pack "Gag has been successfully added!"))
    else ok $ toResponse (pageTemplate "HaskGag – adding a gag" $ H.toHtml (pack "Invalid file type. Only images are allowed."))

-- |Controller for processing delete request
processRemovePage :: ServerPart Response
processRemovePage = path $ \(msg :: String) -> do
    conn <- liftIO $ openConnection
    liftIO $ deleteRow conn (read msg)
    liftIO $ closeConnection conn
    ok $ toResponse (pageTemplate "HaskGag – removing a gag" $ H.toHtml (pack "Gag has been successfully removed!"))