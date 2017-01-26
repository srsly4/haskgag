{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ModelObjects where
import Data.Text

-- |The template gag type. Contains every field gag's used in templates
data Gag = MkGag {
    -- |The id from database
    id :: Int,
    -- |Gag's header
    title :: Text,
    -- |Relative path to the gag's image
    image :: Text }
    deriving Show

-- |Comparing all the attributes
instance Eq Gag where
    (==) MkGag { ModelObjects.id=id1, title = title1, image = image1 }
     MkGag { ModelObjects.id=id2, title = title2, image = image2 }
     = id1 == id2 && title1 == title2 && image1 == image2