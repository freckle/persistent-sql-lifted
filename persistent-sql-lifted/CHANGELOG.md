## [_Unreleased_](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.4.3.0...main)

## [v0.4.3.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.4.2.0...persistent-sql-lifted-v0.4.3.0)

Add `updateGetEntity`

## [v0.4.2.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.4.1.0...persistent-sql-lifted-v0.4.2.0)

Add module `Database.Persist.Sql.Lifted.Savepoint`

## [v0.4.1.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.4.0.0...persistent-sql-lifted-v0.4.1.0)

Add module `Database.Persist.Sql.Lifted.Expression.ArrayAggregate.PostgreSQL`

## [v0.4.0.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.3.0.0...persistent-sql-lifted-v0.4.0.0)

Remove `selectKeys` because Persistent's Conduit-based utilities are of dubious correctness.

Add:

- `deleteWhereCount`
- `existsBy`
- `getFieldName`
- `getTableName`
- `insertUnique_`
- `rawExecute`
- `rawExecuteCount`
- `rawSql`
- `updateWhereCount`

## [v0.3.0.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.2.0.0...persistent-sql-lifted-v0.3.0.0)

Remove `rand`; supports `esqueleto-3.6`.

## [v0.2.0.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.1.1.0...persistent-sql-lifted-v0.2.0.0)

New modules:

- `Database.Persist.Sql.Lifted.Expression`
- `Database.Persist.Sql.Lifted.Expression.Bool`
- `Database.Persist.Sql.Lifted.Expression.Case`
- `Database.Persist.Sql.Lifted.Expression.Comparison`
- `Database.Persist.Sql.Lifted.Expression.Constant`
- `Database.Persist.Sql.Lifted.Expression.Count`
- `Database.Persist.Sql.Lifted.Expression.Exists`
- `Database.Persist.Sql.Lifted.Expression.Insert`
- `Database.Persist.Sql.Lifted.Expression.Key`
- `Database.Persist.Sql.Lifted.Expression.List`
- `Database.Persist.Sql.Lifted.Expression.Maybe`
- `Database.Persist.Sql.Lifted.Expression.Number`
- `Database.Persist.Sql.Lifted.Expression.OrderBy`
- `Database.Persist.Sql.Lifted.Expression.Projection`
- `Database.Persist.Sql.Lifted.Expression.String`
- `Database.Persist.Sql.Lifted.Expression.SubSelect`
- `Database.Persist.Sql.Lifted.Expression.Table`
- `Database.Persist.Sql.Lifted.Expression.Type`
- `Database.Persist.Sql.Lifted.Expression.Update`
- `Database.Persist.Sql.Lifted.Filter`
- `Database.Persist.Sql.Lifted.From`
- `Database.Persist.Sql.Lifted.Query`
- `Database.Persist.Sql.Lifted.Query.Aggregate`
- `Database.Persist.Sql.Lifted.Query.CommonTableExpressions`
- `Database.Persist.Sql.Lifted.Query.Core`
- `Database.Persist.Sql.Lifted.Query.Locking`
- `Database.Persist.Sql.Lifted.Query.SetOperations`
- `Database.Persist.Sql.Lifted.Query.Update`
- `Database.Persist.Sql.Lifted.Update`

## [v0.1.1.0](https://github.com/freckle/persistent-sql-lifted/compare/persistent-sql-lifted-v0.1.0.0...persistent-sql-lifted-v0.1.1.0)

New:

- `transactionSave`
- `transactionSaveWithIsolation`
- `transactionUndo`
- `transactionUndoWithIsolation`

## [v0.1.0.0](https://github.com/freckle/persistent-sql-lifted/tree/persistent-sql-lifted-v0.1.0.0/persistent-sql-lifted)

First release, sprouted from `freckle-app-1.20.3.0`.
