{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module Repo.DB(openDB,closeDB,getDB,putDB) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import qualified Data.Map             as M
import           Data.SafeCopy
import           Data.Typeable
import           Data.Typed
import           System.FilePath

type DB = AcidState DBState

data DBState = DBState !(M.Map AbsRef AbsADT)
             deriving (Typeable,Show)

-- Transactions
whole :: Query DBState DBState
whole = ask

insert :: AbsRef -> AbsADT -> Update DBState ()
insert key value = modify (\(DBState st) -> DBState (M.insert key value st))

getByRef :: AbsRef -> Query DBState (Maybe AbsADT)
getByRef key = asks (\(DBState st) -> M.lookup key st)

makeAcidic ''DBState ['whole,'insert,'getByRef]

wholeDB :: DB -> IO DBState
wholeDB db = query db Whole

emptyDB = DBState M.empty

openDB :: FilePath -> IO DB
openDB dir = do
    db <- openLocalStateFrom (dbDir dir) emptyDB
    -- wholeDB db >>= print
    createCheckpoint db
    return db

getDB :: DB -> AbsRef -> IO (Maybe AbsADT)
getDB db k = query db (GetByRef k)

putDB :: DB -> AbsRef -> AbsADT -> IO ()
putDB db k v = update db (Insert k v)

dbDir dir = dir </> "DB"

closeDB = closeAcidState

$(deriveSafeCopy 0 'base ''Type)
$(deriveSafeCopy 0 'base ''Ref)
$(deriveSafeCopy 0 'base ''ConTree)
$(deriveSafeCopy 0 'base ''ADTRef)
$(deriveSafeCopy 0 'base ''ADT)
$(deriveSafeCopy 0 'base ''NonEmptyList)
$(deriveSafeCopy 0 'base ''DBState)
