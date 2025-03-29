module Database.Persist.Sql.Lifted.Savepoint
  ( rollbackWhen
  ) where

import Prelude ((-))

import Control.Applicative (pure)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (Bool)
import Data.Char (Char)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.Persist.Sql.Lifted.MonadSqlBackend (MonadSqlBackend)
import Database.Persist.Sql.Lifted.Persistent (rawExecute)
import GHC.Stack (HasCallStack)
import System.Random (randomRIO)

--  | Create a new transaction @SAVEPOINT@, returning its name
newSavepoint :: forall m. (HasCallStack, MonadSqlBackend m) => m Text
newSavepoint = do
  r <- replicateM 8 randomCharacter
  let savepoint = "savepoint_" <> T.pack r
  rawExecute ("SAVEPOINT " <> savepoint) []
  pure savepoint

randomCharacter :: forall m. MonadIO m => m Char
randomCharacter = (characterSet V.!) <$> randomRIO (0, V.length characterSet - 1)

characterSet :: Vector Char
characterSet = V.fromList $ ['a' .. 'z'] <> ['1' .. '9']

--  | Release a @SAVEPOINT@
releaseSavepoint :: forall m. (HasCallStack, MonadSqlBackend m) => Text -> m ()
releaseSavepoint name = rawExecute ("RELEASE SAVEPOINT " <> name) []

--  | Rollback to a @SAVEPOINT@
rollbackToSavepoint
  :: forall m. (HasCallStack, MonadSqlBackend m) => Text -> m ()
rollbackToSavepoint name = rawExecute ("ROLLBACK TO SAVEPOINT " <> name) []

-- | Runs a SQL action with SAVEPOINT, rolling back when specified
rollbackWhen
  :: forall m a
   . (HasCallStack, MonadSqlBackend m)
  => (a -> Bool)
  -- ^ When to ROLLBACK based on the result of the action
  -> m a
  -- ^ The action to be run
  -> m a
rollbackWhen shouldRollback act = do
  savepoint <- newSavepoint
  a <- act
  if shouldRollback a
    then rollbackToSavepoint savepoint
    else releaseSavepoint savepoint
  pure a
