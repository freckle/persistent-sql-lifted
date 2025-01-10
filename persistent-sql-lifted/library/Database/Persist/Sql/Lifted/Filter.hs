module Database.Persist.Sql.Lifted.Filter
  ( -- * Type
    Filter

    -- * Equality
  , (==.)
  , (!=.)

    -- * Less & greater
  , (<.)
  , (>.)
  , (<=.)
  , (>=.)

    -- * Lists
  , (<-.)
  , (/<-.)

    -- * Disjunction
  , (||.)
  ) where

import Database.Persist
