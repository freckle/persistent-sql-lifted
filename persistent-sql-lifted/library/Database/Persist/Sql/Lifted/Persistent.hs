{-# LANGUAGE CPP #-}

-- | Wrappers that apply 'liftSql' to Persistent utilities of the same name.
module Database.Persist.Sql.Lifted.Persistent
  ( checkUnique
  , checkUniqueUpdateable
  , count
  , delete
  , deleteBy
  , deleteWhere
  , deleteWhereCount
  , exists
  , existsBy
  , get
  , getBy
  , getByValue
  , getEntity
  , getFieldName
  , getJust
  , getJustEntity
  , getMany
  , getTableName
  , insert
  , insert_
  , insertBy
  , insertEntity
  , insertEntityMany
  , insertKey
  , insertMany
  , insertMany_
  , insertRecord
  , insertUnique
  , insertUnique_
  , insertUniqueEntity
  , onlyUnique
  , putMany
  , rawExecute
  , rawExecuteCount
  , rawSql
  , replace
  , replaceUnique
  , repsert
  , repsertMany
  , selectFirst
  , selectKeysList
  , selectList
  , transactionSave
  , transactionSaveWithIsolation
  , transactionUndo
  , transactionUndoWithIsolation
  , update
  , updateGet
  , updateWhere
  , updateWhereCount
  , upsert
  , upsertBy
  ) where

import Data.Bool (Bool)
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int, Int64)
import Data.Map.Strict (Map)
import Data.Maybe (Maybe)
#if MIN_VERSION_base(4,17,0)
import Data.Type.Equality (type (~))
#endif
import Data.Text (Text)
import Database.Persist
  ( AtLeastOneUniqueKey
  , Entity
  , Filter
  , OnlyOneUniqueKey
  , PersistEntity (..)
  , PersistValue
  , SelectOpt
  , Update
  )
import Database.Persist.Class qualified as P
import Database.Persist.Class.PersistEntity (SafeToInsert)
import Database.Persist.Sql (IsolationLevel, RawSql)
import Database.Persist.Sql qualified as P
import Database.Persist.Sql.Lifted.Core (MonadSqlBackend, SqlBackend, liftSql)
import GHC.Stack (HasCallStack)

-- | Check whether there are any conflicts for unique keys with this entity
--   and existing entities in the database
checkUnique
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => a
  -> m (Maybe (Unique a))
  -- ^ 'Nothing' if the entity would be unique, and could thus safely
  --   be inserted. On a conflict, 'Just' the conflicting key.
checkUnique a = liftSql $ P.checkUnique a

-- | Check whether there are any conflicts for unique keys with this entity and existing entities in the database
--
-- This is useful for updating because it ignores conflicts when the particular entity already exists.
checkUniqueUpdateable
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Entity a
  -> m (Maybe (Unique a))
  -- ^ 'Nothing' if the entity would stay unique, and could thus safely be updated.
  --   On a conflict, 'Just' the conflicting key.
checkUniqueUpdateable e = liftSql $ P.checkUniqueUpdateable e

-- | The total number of records fulfilling the given criteria
count
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m Int
count fs = liftSql $ P.count fs

-- | Delete a specific record by identifier
--
-- Does nothing if record does not exist.
delete
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> m ()
delete k = liftSql $ P.delete k

-- | Delete a specific record by unique key
--
-- Does nothing if no record matches.
deleteBy
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Unique a
  -> m ()
deleteBy u = liftSql $ P.deleteBy u

-- | Delete all records matching the given criteria
deleteWhere
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m ()
deleteWhere fs = liftSql $ P.deleteWhere fs

-- | Delete all records matching the given criteria
deleteWhereCount
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m Int64
  -- ^ The number of rows affected
deleteWhereCount fs = liftSql $ P.deleteWhereCount fs

-- | Check if there is at least one record fulfilling the given criteria
exists
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m Bool
exists fs = liftSql $ P.exists fs

-- | Check if a record with this unique key exists
existsBy
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Unique a
  -> m Bool
existsBy u = liftSql $ P.existsBy u

-- | Get a record by identifier, if available
get
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> m (Maybe a)
get k = liftSql $ P.get k

-- | Get a record by unique key, if available, returning both the identifier and the record
getBy
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Unique a
  -> m (Maybe (Entity a))
getBy u = liftSql $ P.getBy u

-- Get a record by unique key, if available, returning both the identifier and the record
--
-- This function makes the most sense on entities with a single 'Unique' constructor.
getByValue
  :: forall a m
   . ( AtLeastOneUniqueKey a
     , HasCallStack
     , MonadSqlBackend m
     , PersistEntityBackend a ~ SqlBackend
     )
  => a
  -> m (Maybe (Entity a))
  -- ^ A record matching one of the unique keys.
getByValue a = liftSql $ P.getByValue a

-- | Get a record by identifier, if available
getEntity
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> m (Maybe (Entity a))
getEntity k = liftSql $ P.getEntity k

-- | Get the SQL string for the field that an 'EntityField' represents
getFieldName
  :: forall a t m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => EntityField a t
  -> m Text
getFieldName f = liftSql $ P.getFieldName f

-- | Get a record by identifier, if available, for a non-null (not 'Maybe') foreign key
--
-- Unsafe unless your database is enforcing that the foreign key is valid.
getJust
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> m a
getJust k = liftSql $ P.getJust k

-- | Get a record by identifier, if available, for a non-null (not 'Maybe') foreign key
--
-- Unsafe unless your database is enforcing that the foreign key is valid.
getJustEntity
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> m (Entity a)
getJustEntity k = liftSql $ P.getJustEntity k

-- | Get many records by their respective identifiers, if available
getMany
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Key a]
  -> m (Map (Key a) a)
getMany ks = liftSql $ P.getMany ks

-- | Get the SQL string for the table that a 'PersistEntity' represents
getTableName
  :: forall a m. (HasCallStack, MonadSqlBackend m, PersistEntity a) => a -> m Text
getTableName x = liftSql $ P.getTableName x

-- | Create a new record in the database
insert
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m (Key a)
  -- ^ The auto-increment ID that was generated
insert a = liftSql $ P.insert a

-- | Create a new record in the database
insert_
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m ()
insert_ a = liftSql $ P.insert_ a

-- | Insert a value, checking for conflicts with any unique constraints
insertBy
  :: forall a m
   . ( AtLeastOneUniqueKey a
     , HasCallStack
     , MonadSqlBackend m
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m (Either (Entity a) (Key a))
  -- ^ If a duplicate exists in the database, it is returned as 'Left'.
  --   Otherwise, the new 'Key' is returned as 'Right'.
insertBy a = liftSql $ P.insertBy a

-- | Create a new record in the database, returning an auto-increment ID and the inserted record
insertEntity
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m (Entity a)
insertEntity a = liftSql $ P.insertEntity a

-- | Create multiple records in the database, with specified keys
insertEntityMany
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Entity a]
  -> m ()
insertEntityMany es = liftSql $ P.insertEntityMany es

-- | Create a new record in the database using the given key
insertKey
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> a
  -> m ()
insertKey k a = liftSql $ P.insertKey k a

-- | Create multiple records in the database and return their 'Key's
insertMany
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => [a]
  -> m [Key a]
insertMany as = liftSql $ P.insertMany as

-- | Create multiple records in the database
insertMany_
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => [a]
  -> m ()
insertMany_ as = liftSql $ P.insertMany_ as

-- | Create a new record in the database
insertRecord
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m a
  -- ^ The record that was inserted
insertRecord a = liftSql $ P.insertRecord a

-- | Create a new record in the database
insertUnique
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m (Maybe (Key a))
  -- ^ An auto-increment ID, or 'Nothing' when the record couldn't be
  --   inserted because of a uniqueness constraint
insertUnique a = liftSql $ P.insertUnique a

-- | Create a new record in the database
insertUnique_
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m (Maybe ())
  -- ^ (), or 'Nothing' when the record couldn't be inserted because of a
  --   uniqueness constraint
insertUnique_ a = liftSql $ P.insertUnique_ a

-- | Create a new record in the database
insertUniqueEntity
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -> m (Maybe (Entity a))
  -- ^ An auto-increment ID and the inserted record, or 'Nothing' when the record
  --   couldn't be inserted because of a uniqueness constraint.
insertUniqueEntity a = liftSql $ P.insertUniqueEntity a

-- | Return the single unique key for a record
onlyUnique
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , OnlyOneUniqueKey a
     , PersistEntityBackend a ~ SqlBackend
     )
  => a
  -> m (Unique a)
onlyUnique a = liftSql $ P.onlyUnique a

-- | Put many records into the database
--
-- * Insert new records that do not exist (or violate any unique constraints);
-- * Replace existing records (matching any unique constraint).
putMany
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => [a]
  -- ^ A list of the records you want to insert or replace.
  -> m ()
putMany as = liftSql $ P.putMany as

-- | Execute a raw SQL statement
rawExecute
  :: forall m
   . (HasCallStack, MonadSqlBackend m)
  => Text
  -- ^ SQL statement, possibly with placeholders
  -> [PersistValue]
  -- ^ Values to fill the placeholders
  -> m ()
rawExecute t vs = liftSql $ P.rawExecute t vs

-- | Execute a raw SQL statement
rawExecuteCount
  :: forall m
   . (HasCallStack, MonadSqlBackend m)
  => Text
  -- ^ SQL statement, possibly with placeholders
  -> [PersistValue]
  -- ^ Values to fill the placeholders
  -> m Int64
  -- ^ The number of rows modified
rawExecuteCount t vs = liftSql $ P.rawExecuteCount t vs

rawSql
  :: forall a m
   . (HasCallStack, MonadSqlBackend m, RawSql a)
  => Text
  -- ^ SQL statement, possibly with placeholders
  -> [PersistValue]
  -- ^ Values to fill the placeholders
  -> m [a]
rawSql sql vals = liftSql $ P.rawSql sql vals

-- | Replace the record in the database with the given key
--
-- The result is undefined if such record does not exist.
replace
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> a
  -> m ()
replace k a = liftSql $ P.replace k a

-- | Attempt to replace the record of the given key with the given new record
--
-- First query the unique fields to make sure the replacement maintains uniqueness constraints.
replaceUnique
  :: forall a m
   . ( Eq (Unique a)
     , HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> a
  -> m (Maybe (Unique a))
  -- ^ 'Nothing' if the replacement was made. If uniqueness is violated,
  --   'Just' the 'Unique' violation.
replaceUnique k a = liftSql $ P.replaceUnique k a

-- | Put the record in the database with the given key
--
-- If a record with the given key does not exist then a new record will be inserted.
repsert
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> a
  -> m ()
repsert k a = liftSql $ P.repsert k a

-- | Put many entities into the database
--
-- For each item, if a record with the given key does not exist then a new record will be inserted.
repsertMany
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [(Key a, a)]
  -> m ()
repsertMany kas = liftSql $ P.repsertMany kas

-- | Get just the first record for the criteria
selectFirst
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> m (Maybe (Entity a))
selectFirst fs os = liftSql $ P.selectFirst fs os

-- | Get the 'Key's of all records matching the given criteria
selectKeysList
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> m [Key a]
selectKeysList fs os = liftSql $ P.selectKeysList fs os

selectList
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> m [Entity a]
  -- ^ Entities corresponding to the filters and options provided
selectList fs os = liftSql $ P.selectList fs os

-- | Commit the current transaction and begin a new one
transactionSave :: forall m. (HasCallStack, MonadSqlBackend m) => m ()
transactionSave = liftSql P.transactionSave

-- | Commit the current transaction and begin a new one
transactionSaveWithIsolation
  :: forall m
   . (HasCallStack, MonadSqlBackend m)
  => IsolationLevel
  -- ^ Isolation level
  -> m ()
transactionSaveWithIsolation il = liftSql $ P.transactionSaveWithIsolation il

-- | Roll back the current transaction and begin a new one
transactionUndo :: forall m. (HasCallStack, MonadSqlBackend m) => m ()
transactionUndo = liftSql P.transactionUndo

-- | Roll back the current transaction and begin a new one
transactionUndoWithIsolation
  :: forall m
   . (HasCallStack, MonadSqlBackend m)
  => IsolationLevel
  -- ^ Isolation level
  -> m ()
transactionUndoWithIsolation il = liftSql $ P.transactionUndoWithIsolation il

-- | Update individual fields on a specific record
update
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> [Update a]
  -> m ()
update k us = liftSql $ P.update k us

-- | Update individual fields on a specific record, and retrieve the updated value from the database
--
-- This function will throw an exception if the given key is not found in the database.
updateGet
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => Key a
  -> [Update a]
  -> m a
updateGet k us = liftSql $ P.updateGet k us

-- | Update individual fields on any record matching the given criteria
updateWhere
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [Update a]
  -> m ()
updateWhere fs us = liftSql $ P.updateWhere fs us

-- | Update individual fields on any record matching the given criteria
updateWhereCount
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [Update a]
  -> m Int64
  -- ^ The number of rows affected
updateWhereCount fs us = liftSql $ P.updateWhereCount fs us

-- | Update based on a uniqueness constraint or insert:
--
-- * Unsert the new record if it does not exist;
-- * If the record exists (matched via it's uniqueness constraint), then update the
--   existing record with the parameters which is passed on as list to the function.
upsert
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , OnlyOneUniqueKey a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => a
  -- ^ New record to insert
  -> [Update a]
  -- ^ Updates to perform if the record already exists
  -> m (Entity a)
  -- ^ The record in the database after the operation
upsert a us = liftSql $ P.upsert a us

-- | Update based on a given uniqueness constraint or insert:
--
-- * Insert the new record if it does not exist;
-- * Update the existing record that matches the given uniqueness constraint.
upsertBy
  :: forall a m
   . ( HasCallStack
     , MonadSqlBackend m
     , PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     )
  => Unique a
  -- ^ Uniqueness constraint to find by
  -> a
  -- ^ New record to insert
  -> [Update a]
  -- ^ Updates to perform if the record already exists
  -> m (Entity a)
  -- ^ The record in the database after the operation
upsertBy u a us = liftSql $ P.upsertBy u a us
