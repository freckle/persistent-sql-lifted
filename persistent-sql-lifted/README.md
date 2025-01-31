# persistent-sql-lifted

This package introduces two classes:

- `MonadSqlBackend m`, for monadic contexts `m` in which a `SqlBackend` is available

- `MonadSqlBackend db m`, for monadic contexts `m` in which we can execute a SQL
  transaction of type `db a` and get a result `m a`. (The type `db` should have an
  instance of `MonadSqlBackend`.)

Additionally, this package provides variants of query-running utilities from
[persistent] and [esqueleto] which are

1. Concretized to use `SqlBackend`;
2. Generalized to a `MonadSqlBackend m` constraint rather than `ReaderT backend m`;
3. Wrapped in [checkpointCallStack] so that exceptions will include call stacks.

How to migrate from vanilla persistent/esqueleto:

- Instead of [SqlPersistT], use a `MonadSqlBackend` constraint.
- Define an instance of `MonadSqlTx` for your application Monad that specifies how
  your application runs database transactions, e.g. by running [runSqlPool].
- Instead of calling `runSqlPool` directly from the rest of your application code,
  use the `runSqlTx` method from the `MonadSqlTx` class.

For constructing SQL expressions, you may which to import the utilities from
`Database.Persist.Sql.Lifted.Expression` et al rather than getting them from
Esqueleto. This allows you to import the specific bits you need piecemeal and
without having to hide the unlifted versions of query-running functions that this
package replaces. Moreover, this package contains some additional utilities.

[checkpointCallStack]: https://hackage.haskell.org/package/annotated-exception-0.3.0.2/docs/Control-Exception-Annotated-UnliftIO.html
[esqueleto]: https://hackage.haskell.org/package/esqueleto
[persistent]: https://hackage.haskell.org/package/persistent
[runSqlPool]: https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Sql.html#v:runSqlPool
[SqlPersistT]: https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Sql.html#t:SqlPersistT
