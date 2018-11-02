# persistent-template-classy

Generate classy lens for your Persistent fields. For example:

```haskell
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
```

This would essentially generate this additional code:

```haskell
class HasName ev a | ev -> a where
  name :: Lens' ev a
class HasAge ev a | ev -> a where
  age :: Lens' ev a
class HasTitle ev a | ev -> a where
  title :: Lens' ev a
class HasAuthorId ev a | ev -> a where
  authorId :: Lens' ev a

instance HasName Person String where
  name = (lens personName) (\ x y -> x {personName = y})
instance HasAge Person Int where
  age = (lens personAge) (\ x y -> x {personAge = y})
instance HasTitle BlogPost String where
  title = (lens blogPostTitle) (\ x y -> x {blogPostTitle = y})
instance HasAuthorId BlogPost PersonId where
  authorId
    = (lens blogPostAuthorId) (\ x y -> x {blogPostAuthorId = y})
```

Class-generation is separated from instance-generation because it's
wise to keep it in a single place in your project and reuse with a
non-persistent code.
