# OAKEM

Oakem is an application that helps to work with database. It consists of two main parts:

* oakpool, organizes connection pool and provides a way to make a query
* oakem, combines most common operations with db-records, e.g. CRUD and pagination access

It is build on the top of Erlang ODBC API and currently supports only T-SQL dialect

## OAKPOOL

### Overview

All db-queries could be separated into two types: which require and which does not require an open transaction.
According to this oakpool provides two sets of functions: transact_funname and atomic_funname respectively.
Config file could contain description for several pools, check this config snippet:

```erlang
{pools, [
    {atomic_pool1, [
        {query_max_time, 60000}, 
        {size, 5},
        {connection_string, "DSN=odbc-sql-server1;UID=username;PWD=passwd"},
        {auto_commit, on}
    ]},
    {atomic_pool2, [
        {query_max_time, 60000}, 
        {size, 20},
        {connection_string, "DSN=odbc-sql-server2;UID=username;PWD=passwd"},
        {auto_commit, on}
    ]},
    {transact_pool1, [
        {query_max_time, 60000}, 
        {size, 10},
        {connection_string, "DSN=odbc-sql-server3;UID=username;PWD=passwd"},
        {auto_commit, off}
    ]}
]}
```

During application startup top-supervisor creates separate sub-supervisors for each of this pool.
Each sub-supervisor spawns workers. All this workers registers themselves in two different process groups:
`oakpool_tpg` (for transact workers with `{auto_commit, off}` option) and `oakpool_apg` (otherwise).
All atomic queries handled by workers in `oakpool_apg` process group. All transact queries are first
should be delegated by `oakmanager` (coordinator-process), after this worker could be accessed by callee directly 
for some time (`query_max_time` option for this pool).

### Atomic query example

```erlang
1> oakpool:atomic_query("SELECT 123 as hello").
{selected,["hello"], [{123}]}
```
### Transact query example

```erlang
1> TRef = oakpool:begin_transaction().                                                   
<0.89.0>
2> oakpool:transact_query(TRef, "INSERT INTO t1(f1) VALUES (?)", [{sql_integer, [123]}]).
{updated,1}
3> oakpool:commit(TRef).
ok
```
### Debug
Oakpool could be traced, it has some probes. Try following to watch all process alive:

```erlang
et_viewer:start([{trace_global, true}, {trace_pattern, {et,max}}]).
```

## OAKEM 

### Overview

oakem it is attempt to structurize most common operations with database and give flexible
datastructures for it.

### Entities

All data accessible via so called "entities". Entity it is synonim for table row but with
additional metadata inside. Internal representation of entity is #db_entity record. You
could dive into `include/oakem.hrl` for additional knowlege.

During `oakem` application startup it looks into its env, takes `{entities, [...]}` section, transforms
it to `#db_entity` record list and compiles on the fly into `oakentity_hlp` module.
Look at `oakentity` module for further API information. Basic entity's examples could be found in `src/oakem.app.src`.
Also all entities supports unidirectional links between eachother.

### OAKEM API

CRUD operations:

* oakem:insert/1
* oakem:select/1
* oakem:update/1
* oakem:delete/1

Pagination access:

* oakem:get_total/2
* oakem:get_page/6

## DOCs

All docs could could be generated via edoc subsistem.
