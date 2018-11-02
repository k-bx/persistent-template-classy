{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Lens (Lens', (^.), lens)
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.TH.Classy

$(mkClassyClass "name")
$(mkClassyClass "age")
$(mkClassyClass "title")
$(mkClassyClass "authorId")

share
  [mkPersist sqlSettings, mkMigrate "migrateAll", mkClassyInstances]
  [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main =
  runSqlite ":memory:" $ do
    runMigration migrateAll
    let johnDoe = Person "John Doe" $ Just 35
    johnId <- insert johnDoe
    liftIO $ print $ johnDoe ^. name
    liftIO $ print $ johnDoe ^. age
    let post = BlogPost "My fr1st p0st" johnId
    liftIO $ print $ post ^. title

