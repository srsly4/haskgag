{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Templates where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Data.Text (Text, pack, unpack, concat)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import ModelObjects

-- |Main template for the page structure (headers, containers etc.)
pageTemplate :: Text -> Html -> Html
pageTemplate title body = do
    H.html $ do
        H.head $ do
            H.title (toHtml $ Data.Text.concat [title, " - haskgag"])
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/foundation.min.css"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/app.css"
            H.base ! A.href "/"
        H.body $ do
            H.div ! A.class_ "top-bar" $ do
                H.div ! A.class_ "top-bar-left" $ do
                    H.ul ! A.class_ "dropdown menu" $ do
                        H.li ! A.class_ "menu-text" $ toHtml (pack "HaskGag")
                        H.li $ H.a ! A.href "/" $ toHtml (pack "Homepage")
                        H.li $ H.a ! A.href "/add" $ toHtml (pack "Add a gag")
            H.div ! A.id "container" ! A.class_ "row" $ H.div ! A.class_ "large-6 large-centered medium-8 medium-centered small-12 column center" $ body
            H.script (toHtml $ pack "") ! A.type_ "text/javascript" ! A.src "/static/js/vendor/jquery.js"
            H.script (toHtml $ pack "") ! A.type_ "text/javascript" ! A.src "/static/js/vendor/foundation.min.js"

-- |Template for the single gag
gagTemplate :: Gag -> Html
gagTemplate gag = do
    H.div ! A.class_ "gag" ! A.id (toValue (show $ ModelObjects.id gag)) $ do
        H.h3 $ toHtml $ ModelObjects.title gag
        H.img ! A.src (toValue $ ModelObjects.image gag)
        H.div ! A.class_ "gagops" $ do
            H.a ! A.href (toValue $ "removegag/" ++ (show $ ModelObjects.id gag)) $ toHtml ("Remove" :: Data.Text.Text)

-- |Template for the list of gags
gagListTemplate :: [Gag] -> Html
gagListTemplate [] = do
    H.div ! A.class_ "notfound" $ (toHtml (pack "There are not any gags in database yet. Maybe want to add one?"))
gagListTemplate gags = do
    H.div ! A.class_ "gaglist" $ (toHtml $ (Prelude.map gagTemplate gags))

-- |Adding a gag form template
gagAddTemplate :: Html
gagAddTemplate = do
    H.form ! A.action "/processadd" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
        H.h1 $ "Add new gag"
        H.label ! A.for "title" $ "Title of created gag"
        H.input ! A.type_ "text" ! A.placeholder "Ex. throwing sausages" ! A.id "title" ! A.name "title"
        H.input ! A.type_ "file" ! A.id "image" ! A.name "image" ! A.size "40"
        H.input ! A.type_ "submit" ! A.value "Send it!"