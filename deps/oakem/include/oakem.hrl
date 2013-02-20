-define(DB_TIMEOUT, 30000).

-type keyvalue() :: [{string(), any()}].

-record(db_field, {
    name        = undefined :: string(),
    select      = undefined :: string(),
    mutable     = true      :: boolean() ,
    sql_type    = sql_integer   :: odbc:odbc_data_type(),
    value       = null      :: odbc:value(),
    key         = false     :: boolean()
}).

-record(db_entity, {
    name        = undefined :: atom(),
    schema      = undefined :: string(),
    table       = undefined :: string(),
    fields      = []        :: [#db_field{}],
    deps        = []        :: [any()]
}).

%% @doc Describes connection between two different entities. This
%%      link could have different types: `one_to_many | many_to_one | many_to_many | one_to_one'
%%      Each type corresponding to it`s E-R analogue. E.g. we have two
%%      sql tables: `providers(p_id, p_name)' and `consumers(c_id, c_name, p_id)'. This two tables has
%%      `one_to_many' link type, if we look from provider's side, and `many_to_one', if we look from
%%      consumer's side.
%%      In 1st case we could add such link to provider's entity:
%%      `#db_link{name = "consumers", entity = consumer, fields = ["p_id"], type = one_to_many}'.
%%      In 2nd case we could add scuh link to consumer's entity:
%%      `#db_link{name = "provider", entity = provider, fields = ["p_id"], type = many_to_one}'.
%%      If we extend our provider's entity like this: `provider(p_id, p_name, p_parent_id)',
%%      then we could add such 2 links to provider's entity:
%%      `#db_link{name = "parent", entity = provider, fields = [{"p_parent_id", "p_id"}], type = many_to_one}',
%%      `#db_link{name = "children", entity = provider, fields = [{"p_id", "p_parent_id"}], type = one_to_many}'.
%%
%%      Suppose we have other connection with table `regions(r_id, r_name)'. One provider could have their office
%%      in different regions, and different regions could have many providers. In sql this link have additional
%%      connection table: `providers_regions(p_id, r_id)'. Now we could add such link to provider's entity:
%%      `#db_link{name = "regions", entity = region, fields = [{src, "p_id"}, {dst, "r_id"}], type = many_to_many, via = "providers_regions"}'.
%%      In this case `src' fields taken from provider's entity and `dst' fields taken from region's entity. Both
%%      types should exists in connection table.
%%      If for some reason this connection table have additional fields and different names
%%      (e.g. `providers_regions(prov_id, reg_id, extra_cost)') we could choose another names for it and
%%      add this field to incoming entities:
%%      `...fields = [{src, {"p_id", "prov_id"}}, {dst, {"reg_id", "r_id"}}], via = {"providers_regions", ["extra_cost"]}...'.
%%
-record(db_link, {
    name        = undefined :: string(),
    entity      = undefined :: atom(),
    fields      = []        :: [
        Field   :: string() | {Src :: string(), Dst :: string()} | % for one_to_one/one_to_many/many_to_one
        {Side :: src | dst, Field :: string() | {Src :: string(), Dst :: string()}} % for many_to_many
    ],
    type        = undefined :: one_to_many | many_to_one | many_to_many | one_to_one,
    via         = undefined :: {Table :: string(), ExtFields :: [string()]}
}).
