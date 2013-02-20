%% @author Churikov Daniil <ddosia@gmail.com>
%% @doc Common interface which allows to do basic operations
%%      with db records (for example CRUD, pagination access, etc).
-module(oakem).

-include("oakem.hrl").

%% API
-export([
    get_total/2, get_page/6,
    insert/1, select/1, delete/1, update/1
]).


-spec select(E :: #db_entity{}) -> #db_entity{} | false | {error, string()}.
%% @doc Fetches entity from DB.
select(Entity = #db_entity{fields = Fields}) ->
    SStr = oakentity:sql_fields(Entity),
    TStr = oakentity:sql_table(Entity),
    {PrepKeyNames, KTVs} = lists:unzip([
        {N ++ case V of null -> " IS null"; _ -> " = ?" end, {T, [V]}}
        || #db_field{name = N, value = V, sql_type = T, key = true} <- Fields 
    ]),
    WStr = string:join(PrepKeyNames, " AND "),

    Res = oakpool:atomic_query(
        "SELECT " ++ SStr ++ " FROM " ++ TStr ++ "  WHERE " ++ WStr,
        lists:filter(fun({_T, [V]}) -> V =/= null end, KTVs)
    ),
    case Res of
        {selected, _, []} ->
            false;
        {selected, Cols, [Row]} ->
            oakentity:set_values(lists:zip(Cols, tuple_to_list(Row)), Entity);
        {error, Reason} ->
            {error, Reason}
    end.


-spec insert(E :: #db_entity{}) -> #db_entity{} | {error, string()}.
%% @doc Inserts entity to DB.
%%      Modifies entities with `#db_field.mutable =:= true' only.
insert(Entity = #db_entity{fields = Fields}) ->
    {FieldNames, TypesAndVals} = lists:unzip([
        {N, {T, [V]}}
        || #db_field{name = N, value = V, sql_type = T, mutable = true} <- Fields
    ]),

    TStr = oakentity:sql_table(Entity),
    FStr = string:join(FieldNames, ","),
    VStr = string:join([ case V of null -> "null"; _ -> "?" end || {_T, [V]} <- TypesAndVals ], ","),

    Q = "INSERT INTO " ++ TStr ++ "(" ++ FStr ++ ") VALUES( " ++ VStr ++ " )",
    KeyQ = case oakentity:get_keyvalues(Entity) of
        [{Key, _Val}] ->
            " SELECT CAST (SCOPE_IDENTITY() AS int) as " ++ Key;
        _ ->
            ""
    end,
    Res = oakpool:atomic_query(
        Q ++ KeyQ,
        lists:filter(fun({_T, [V]}) -> V =/= null end, TypesAndVals)
    ),
    case Res of
        {selected, [K], [{V}]} ->
            oakentity:set_values([{K, V}, {"date_create", null}, {"creator", null}], Entity);
        {updated, 1} ->
            oakentity:set_values([{"date_create", null}, {"creator", null}], Entity);
        {error, Reason} ->
            {error, Reason}
    end.


-spec update(E :: #db_entity{}) -> true | false | {error, string()}.
%% @doc Persists entity changes to DB.
%%      Modify enity fields with `#db_field.mutable =:= true' only.
update(Entity = #db_entity{fields = Fields}) ->
    {PrepFieldNames, TypesAndVals} = lists:unzip([
        {case V of null -> N ++ "=null"; _ -> N ++ "=?" end, {T, [V]}}
        || #db_field{name = N, value = V, sql_type = T, mutable = true} <- Fields
    ]),
    {PrepKeyNames, KTVs} = lists:unzip([
        {N ++ case V of null -> " IS null"; _ -> "=?" end, {T, [V]}}
        || #db_field{name = N, value = V, sql_type = T, key = true} <- Fields 
    ]),

    TStr = oakentity:sql_table(Entity),
    FStr = string:join(PrepFieldNames, ","),
    WStr = string:join(PrepKeyNames, " AND "),
    Res = oakpool:atomic_query(
        "UPDATE " ++ TStr ++ " SET " ++ FStr ++ " WHERE " ++ WStr,
        lists:filter(fun({_T, [V]}) -> V =/= null end, TypesAndVals) ++
        lists:filter(fun({_T, [V]}) -> V =/= null end, KTVs)
    ),
    case Res of
        {updated, 1} -> true;
        {updated, 0} -> false;
        {error, Reason} -> {error, Reason}
    end.


-spec delete(E :: #db_entity{}) -> true | false | {error, string()}.
%% @doc Deletes entity from DB.
%%      When entity was successfully deleted returns true, otherwise false.
delete(Entity = #db_entity{fields = Fields}) ->
    {PrepKeyNames, KTVs} = lists:unzip([
        {N ++ case V of null -> " IS null"; _ -> " = ?" end, {T, [V]}}
        || #db_field{name = N, value = V, sql_type = T, key = true} <- Fields 
    ]),

    KStr = string:join(PrepKeyNames, " AND "),
    TStr = oakentity:sql_table(Entity),
    Res = oakpool:atomic_query(
        "DELETE FROM " ++ TStr ++ " WHERE " ++ KStr,
        lists:filter(fun({_T, [V]}) -> V =/= null end, KTVs)
    ),
    case Res of
        {updated, 1} -> true;
        {updated, 0} -> false;
        {error, Reason} -> {error, Reason}
    end.


-spec get_total(Entity :: atom(), Filters :: [filter()]) -> pos_integer().
%% @doc Applies filters and returns total entity count.
get_total(EntityAtom, Filters) ->
    Entity = oakentity:new(EntityAtom),
    
    TStr = oakentity:sql_table(Entity),
    {FiltersStr, FiltersParams} = prepare_filters(Filters, Entity),

    Res = oakpool:atomic_query(
        "SELECT
          COUNT(*) as total
        FROM " ++ TStr ++ case FiltersStr of "" -> ""; _ -> " WHERE " ++ FiltersStr end,
        FiltersParams
    ),
    case Res of
        {selected, ["total"], [{Total}]} ->
            Total;
        {error, Reason} ->
            {error, Reason}
    end.

-type orderby() :: {string(), asc | desc}.
-type filter() :: {string(), string(), any()}.
-spec get_page(
    Entity :: atom(), Num :: pos_integer(), Size :: pos_integer(),
    Filters :: [filter()], Order :: [orderby()], DepsNames :: [atom()]
) -> [keyvalue()] | {error, string()}.
%% @doc Splits entities into `Size' pages and returns sorted and/or filtred `Num'th page.
%%      `Order' - list in `{Field, Mode}' format, which will be used to construct `ORDER BY' clause.
%%      Mode could be one of the asc | desc.
%%      Filters list in `{Field, "~" | "^" | ":", Value}' format, used to construct `WHERE' clause.
%%      
%%      Returns propper list with such fields: [cols, rows, rows_total, page_count, page_num, page_size, rows_from, rows_to].
get_page(EntityAtom, Num, Size, Filters, Order, DepsNames) ->
    Total = get_total(EntityAtom, Filters),
    case Total of 
        0 ->
            [
                {cols, []}, {rows, []},
                {rows_total, 0}, {page_count, 0}, {page_num, 0},
                {page_size, Size}, {rows_from, 0}, {rows_to, 0}
            ];
        Total when is_integer(Total) ->
            get_page(EntityAtom, Num, Size, Filters, Order, DepsNames, Total)
    end.

get_page(EntityAtom, Num, Size, Filters, Order, DepsNames, Total) ->

    Entity = #db_entity{fields = Fields, deps = Deps} = oakentity:new(EntityAtom),
    UDepsNames = lists:usort(DepsNames),

    Rem = Total rem Size, 
    PageCount = case Rem > 0 of
        true ->
            trunc((Total - Rem) / Size) + 1;
        false ->
            trunc(Total / Size)
    end,
    K = (Size * (Num - 1) + 1),
    M = (Size * Num),

    SStr = oakentity:sql_fields("t0.", Entity),
    OStr = orderby_clause(Entity, Order),
    {DepsSStr, DepsJStr, DepsLengths} = join_clause(UDepsNames, Entity),
    {FiltersStr, FiltersParams} = prepare_filters(Filters, Entity),

    TStr = oakentity:sql_table(Entity),
    Res = oakpool:atomic_query(
        "SELECT " ++ SStr ++
        case DepsSStr of "" -> ""; _ -> "," ++ DepsSStr end ++
            " FROM (
            SELECT
            row_number() OVER (ORDER BY " ++ OStr ++ ") AS rn, *
            FROM " ++ TStr ++ case FiltersStr of [] -> ""; _ -> " WHERE " ++ FiltersStr end ++ "
        ) as t0
        " ++ DepsJStr ++ "
        WHERE rn BETWEEN ? AND ?",
        FiltersParams ++ [{sql_integer, [K]}, {sql_integer, [M]}]
    ),
    case Res of
        {selected, Cols, Rows} ->
            OriginalFieldsWithPos = lists:zip(
                Fields, lists:seq(1, length(Fields))
            ),

            [{origin, OriginCols} | DepsCols] = split_by_map(
                [list_to_binary(C) || C <- Cols], [{origin, length(Fields)} | DepsLengths], []
            ),
            {MultyDepsCols, MultyDeps} = lists:unzip([
                    construct_dep_query(L, OriginalFieldsWithPos) ||
                    L = #db_link{name = JoinName, type = T} <- Deps,
                    (T =:= one_to_many orelse T =:= many_to_many),
                    lists:member(JoinName, UDepsNames) =:= true
                ]),

            CombinedRows = lists:map(
                fun(Row) ->
                        [{origin, OriginRow} | DepsRow] = split_by_map(
                            tuple_to_list(Row), [{origin, length(Fields)} | DepsLengths], []
                        ),
                        MultyDepsRows = lists:map(
                            fun({JoinName, Query, TypesAndPos}) ->
                                    Params = [ {T, [element(P, Row)]} || {T, P} <- TypesAndPos],
                                    case lists:any(fun({_, [null]}) -> true; (_) -> false end, Params) of
                                        true ->
                                            % query will return empty result
                                            {list_to_binary(JoinName), []};
                                        false ->
                                            {selected, _, DepRows} = oakpool:atomic_query(Query, Params),
                                            {list_to_binary(JoinName), [ tuple_to_list(DR) || DR <- DepRows ]}
                                    end
                            end, MultyDeps
                        ),
                        NewDepsRow = lists:map(
                            fun ({JN, DR}) ->
                                case lists:any(fun(null) -> false; (_) -> true end, DR) of
                                    false ->
                                        {JN, null};
                                    true ->
                                        {JN, DR}
                                end
                            end, DepsRow
                        ),
                        
                        [ {deps, NewDepsRow ++ MultyDepsRows} | OriginRow ]
                end,
                Rows
            ),
            [
                {cols, [{deps, DepsCols ++ MultyDepsCols } | OriginCols]},
                {rows, CombinedRows},
                {rows_total, Total}, {page_count, PageCount}, {page_num, Num},
                {page_size, Size}, {rows_from, K}, {rows_to, M}
            ];
        {error, Reason} ->
            {error, Reason}
    end.


%%%===================================================================
%%% Internal
%%%===================================================================

construct_dep_query(#db_link{entity = EAtom, name = JoinName,
        fields = Joins, type = T}, OriginalFieldsWithPos) when T =:= one_to_many ->

    Entity = #db_entity{fields = Fields} = oakentity:new(EAtom),
    SStr = oakentity:sql_fields(Entity),
    TStr = oakentity:sql_table(Entity),
    WStr = string:join([ DstJ ++ "=?" || {_SrcJ, DstJ} <- Joins ], " AND "),

    Q = "SELECT " ++ SStr ++ " FROM " ++ TStr ++ "  WHERE " ++ WStr,
    SelectedFields = [ list_to_binary(N) || #db_field{name = N} <- Fields ],
    TypesAndPos = [
        {OType, Pos} || 
        {SrcJ, _DstJ} <- Joins, % important to follow in `Joins''s order
        { #db_field{name = OField, sql_type = OType}, Pos } <- OriginalFieldsWithPos,
        SrcJ =:= OField
    ],
    {
        {JoinName, SelectedFields},
        {JoinName, Q, TypesAndPos}
    };


construct_dep_query(#db_link{entity = EAtom, name = JoinName, via = {ViaTable, ExtFields},
        fields = Joins, type = T}, OriginalFieldsWithPos) when T =:= many_to_many ->

    Entity = #db_entity{fields = Fields} = oakentity:new(EAtom),
    SStr = oakentity:sql_fields("t0.", Entity),
    ExtSStr = case ExtFields of
        [] ->
            "";
        _ ->
            "," ++ string:join([ "t1." ++ N || N <- ExtFields ], ",")
    end,
    TStr = oakentity:sql_table(Entity),
    ONStr = string:join(["t0." ++ DstJ ++ "=" ++ "t1." ++ ConnJ || {dst, {ConnJ, DstJ}} <- Joins], " AND "),
    WStr = string:join([ "t1." ++ ConnJ ++ " = ?" || {src, {_, ConnJ}} <- Joins ], " AND "),

    Q = "SELECT " ++ SStr ++ ExtSStr ++ " FROM " ++ TStr ++ " as t0 "
    ++ "INNER JOIN " ++ ViaTable ++ " as t1 ON " ++ ONStr ++ "  WHERE " ++ WStr,
    SelectedFields = [ list_to_binary(N) || #db_field{name = N} <- Fields] ++ [ list_to_binary(EF) || EF <- ExtFields],
    TypesAndPos = [
        {OType, Pos} || 
        {src, {SrcJ, _}} <- Joins, % important to follow in `Joins''s order
        { #db_field{name = OField, sql_type = OType}, Pos } <- OriginalFieldsWithPos,
        SrcJ =:= OField
    ],
    {
        {JoinName, SelectedFields},
        {JoinName, Q, TypesAndPos}
    }.



orderby_clause(Entity, []) ->
    {Keys, _} = lists:unzip(oakentity:get_keyvalues(Entity)),
    string:join(Keys, ",");

orderby_clause(Entity = #db_entity{fields = Fields}, Order) when length(Order) > length(Fields) ->
    orderby_clause(Entity, []);

orderby_clause(Entity = #db_entity{fields = Fields}, Order) ->
    ValidOrder = [ O ++ " " ++ atom_to_list(Mode) || {O, Mode} <- Order,
        lists:keyfind(O, #db_field.name, Fields) =/= false,
        (Mode =:= asc orelse Mode =:= desc)
    ],
    case ValidOrder of
        [] ->
            orderby_clause(Entity, []);
        _ ->
            string:join(ValidOrder, ",")
    end.



prepare_filters([], _) ->
    {"", []};
prepare_filters(Filters, #db_entity{fields = Fields}) ->
    {FLst, FPs} = lists:unzip([
        {where_clause(F, S, O), {T, [V]}} ||
        {F, O, V} <- Filters, (O =:= $~ orelse O =:= $^ orelse O =:= $:), 
        #db_field{name = N, select = S, sql_type = T} <- Fields, N =:= F, V =/= null
    ]),
    {string:join(FLst, " AND "), FPs}.



where_clause(Field, undefined, Operand) ->
    where_clause(Field, Operand);
where_clause(_Field, Select, Operand) ->
    where_clause(Select, Operand).

where_clause(Field, $~) ->
    "(lower(" ++ Field ++ ") LIKE ('%' + lower(?) + '%'))";
where_clause(Field, $^) ->
    "(lower(" ++ Field ++ ") NOT LIKE ('%' + lower(?) + '%'))";
where_clause(Field, $:) ->
    Field ++ " = ?".


split_by_map([], [], Res) ->
    lists:reverse(Res);
split_by_map(List, [{Key, Len} | Map], Res) ->
    {Chunk, Rest} = lists:split(Len, List),
    split_by_map(Rest, Map, [{Key, Chunk} | Res]).


join_clause(DepsNames, #db_entity{deps = Deps}) ->
    {DepsSStrs, DepsJStrs, DepsLengths} = lists:unzip3(extract_joins(
        [D || D = #db_link{name = N} <- Deps, lists:member(N, DepsNames) =:= true], []
    )),
    {string:join(DepsSStrs, ","), string:join(DepsJStrs, " "), DepsLengths}.


extract_joins([], Acc) ->
    Acc;
extract_joins([#db_link{type = T} | Deps], Acc) when not (T =:= one_to_one orelse T =:= many_to_one) ->
    extract_joins(Deps, Acc);
extract_joins([#db_link{entity = E, name = DepName, fields = Joins} | Deps], Acc) ->
    DepEntity = #db_entity{fields = DepFields} = oakentity:new(E),
    DepNum = integer_to_list(length(Acc) + 1),
    DepTable = oakentity:sql_table(DepEntity),

    JoinStr = string:join(
        ["t0." ++ SrcJ ++ " = t" ++ DepNum ++ "." ++ DstJ || {SrcJ, DstJ} <- Joins],
        " AND "
    ),
    extract_joins(Deps, [{
        oakentity:sql_fields("t" ++ DepNum ++ ".", DepEntity),
        "LEFT OUTER JOIN " ++ DepTable ++ " as t" ++ DepNum ++ " ON " ++ JoinStr,
        {DepName, length(DepFields)}
    } | Acc]).

