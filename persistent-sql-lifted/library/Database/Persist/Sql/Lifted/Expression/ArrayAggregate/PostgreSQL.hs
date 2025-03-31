module Database.Persist.Sql.Lifted.Expression.ArrayAggregate.PostgreSQL
  ( AggMode (..)
  , arrayAggDistinct
  , arrayAgg
  , arrayAggWith
  , arrayRemove
  , arrayRemoveNull
  , maybeArray
  , arrayAggById
  , arrayAggByIdMaybe
  , arrayAggByMaybe
  , arrayAggBy
  ) where

import Prelude

import Database.Esqueleto.Experimental
  ( Entity
  , EntityField
  , PersistEntity
  , PersistField
  , SqlExpr
  , Value
  , asc
  , persistIdField
  , (^.)
  )
import Database.Esqueleto.Internal.Internal
  ( (??.)
  )
import Database.Esqueleto.PostgreSQL
  ( AggMode (..)
  , arrayAgg
  , arrayAggDistinct
  , arrayAggWith
  , arrayRemove
  , arrayRemoveNull
  , maybeArray
  )

-- | Aggregrate the given column with stable ordering (by ID)
--
-- This ensures that if you aggregrate two columns:
--
-- @
-- pure
--   ( 'arrayAggById' students StudentFirstName
--   , 'arrayAggById' students StudentLastName
--   )
-- @
--
-- The list, if zipped, will be as expected.
--
-- See <https://stackoverflow.com/a/7317520>.
--
-- Also replaces the 'Maybe' result with an empty list, because really that's
-- what you always want.
arrayAggById
  :: forall val typ
   . (PersistEntity val, PersistField [typ], PersistField typ)
  => SqlExpr (Entity val)
  -> EntityField val typ
  -> SqlExpr (Value [typ])
arrayAggById es f = arrayAggBy (es ^. f) (es ^. persistIdField)

-- | 'arrayAggById' but for a left-outer-joined entity
--
-- If you're using '(?.)' instead of '(^.)', use this instead of 'arrayAggById'.
arrayAggByIdMaybe
  :: forall val typ
   . (PersistEntity val, PersistField typ)
  => SqlExpr (Maybe (Entity val))
  -> EntityField val typ
  -> SqlExpr (Value [typ])
arrayAggByIdMaybe es f =
  arrayRemoveNull $ arrayAggBy (es ??. f) (es ??. persistIdField)

-- | 'arrayAggBy' but for a left-outer-joined entity
--
-- If you're using '(?.)' instead of '(^.)', use this instead of 'arrayAggBy'.
arrayAggByMaybe
  :: forall a b
   . (PersistField a, PersistField b)
  => SqlExpr (Value (Maybe a))
  -> SqlExpr (Value b)
  -> SqlExpr (Value [a])
arrayAggByMaybe a = arrayRemoveNull . arrayAggBy a

arrayAggBy
  :: forall a b
   . (PersistField [a], PersistField a, PersistField b)
  => SqlExpr (Value a)
  -> SqlExpr (Value b)
  -> SqlExpr (Value [a])
arrayAggBy a b = maybeArray $ arrayAggWith AggModeAll a [asc b]
