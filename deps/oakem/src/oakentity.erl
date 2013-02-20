%% @author Churikov Daniil <ddosia@gmail.com>
%% @doc Contains entity processing functions.
-module(oakentity).

-include("oakem.hrl").

%% API
-export([
    new/1, new/2, is_supported/1,
    set_values/2,
    get_values/1, get_value/2, get_value/3, get_keyvalues/1,
    sql_fields/1, sql_fields/2, sql_table/1
]).

%% private exports
-export([
    compile_entites/1, str_replace/3
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec set_values(Values :: [keyvalue()], E :: #db_entity{}) -> #db_entity{}.
%% @doc Accepts key/values pairs, searches correspond `#db_field.key' and sets its values.
set_values(Values, E = #db_entity{fields = Fields}) ->
    NewFields = lists:map(
        fun (X = #db_field{name = F}) ->
            case lists:keyfind(F, 1, Values) of
                false -> X;
                {F, V} -> X#db_field{value = V}
            end
        end, Fields
    ),
    E#db_entity{fields = NewFields}.


-spec get_value(Field :: string(), E :: #db_entity{}) -> odbc:value().
%% @doc As get_value/3 but without default value.
get_value(Field, E) ->
    get_value(Field, E, null).

-spec get_value(Field :: string(), E :: #db_entity{}, Default :: odbc:value()) -> odbc:value().
%% @doc Returns `#db_field.value' where key is compares equal to `Field'.
%%      `Default' value returns if value equals to null.
get_value(Field, #db_entity{fields = Fields}, Default) ->
    case lists:keyfind(Field, #db_field.name, Fields) of
        #db_field{value = null} -> Default;
        #db_field{value = Value} -> Value
    end.


-spec get_values(E :: #db_entity{}) -> [keyvalue()].
%% @doc Returns fields as key/value list.
get_values(#db_entity{fields = Fields}) ->
    [ {F, V} || #db_field{name = F, value = V} <- Fields ].

-spec get_keyvalues(E :: #db_entity{}) -> [keyvalue()].
%% @doc Returns only `#db_field.key =:= true' fields as key/value list.
%%      Values also contains it sql_type, e.g. `{SqlType, [Value]}'.
get_keyvalues(#db_entity{fields = Fields}) ->
    [ {F, {T, [V]}} || #db_field{name = F, value = V, sql_type = T, key = K} <- Fields, K =:= true ].

-spec new(Entity :: atom(), Values :: [keyvalue()]) -> #db_entity{}.
%% @doc As new/1 but with presetted key/values.
new(EntityAtom, Values) ->
    E = new(EntityAtom),
    set_values(Values, E).

-spec new(Entity :: atom()) -> #db_entity{}.
%% @doc Creates new entity. Look at `#db_entity{}' record for fields description.
new(EntityAtom) ->
    oakentity_hlp:new(EntityAtom).


-spec is_supported(Name :: atom() | string()) -> atom() | false.
%% @doc Checks this entity is supported or not.
%%      Accepts `Name' as  `#db_entity.name' atom or `#db_entity.table' string.
is_supported(EntityName) ->
    oakentity_hlp:is_supported(EntityName).


-spec sql_fields(E :: #db_entity{}) -> string().
%% @doc As sql_fields/2, but without Alias param
sql_fields(E) ->
    sql_fields("", E).

%% @doc returns field list for 'SELECT field_list FROM ...' sql statement.
%%      Adds alias for all fields (e.g. with Alias == "t1." field1 becomes t1.field1)
-spec sql_fields(Alias :: string(), E :: #db_entity{}) -> string().
sql_fields(TableAlias, #db_entity{fields = Fields}) ->
    string:join(
        [case {S, TableAlias} of
            {undefined, _} -> TableAlias ++ N;
            {S, ""} -> S ++ " AS " ++ N;
            {S, TableAlias} -> str_replace(S, N, TableAlias ++ N) ++ " AS " ++ N
        end || #db_field{select = S, name = N} <- Fields ],
        ","
    ).


-spec sql_table(#db_entity{}) -> string().
%% @doc extracts full table name
sql_table(#db_entity{schema = undefined, table = T}) ->
    T;
sql_table(#db_entity{schema = S, table = T}) ->
    "[" ++ S ++ "].[" ++ T ++ "]".


%%%===================================================================
%%% Internal
%%%===================================================================

str_replace(Where, What, For) ->
    str_replace(Where, What, For, []).

str_replace(Where, What, For, Acc) ->
    case string:str(Where, What) of
        Pos when Pos =:= 0 ->
            Acc ++ Where;
        Pos when Pos > 0 ->
            {BegWhat, Rest} = lists:split(Pos + length(What) - 1, Where),
            Beg = Acc ++ string:substr(BegWhat, 1, Pos - 1) ++ For,
            str_replace(Rest, What, For, Beg)
    end.


%% compile related function VVVVVVVVVV
%% @private
compile_entites(ESpecs) ->
    [db_entity | EDefVals] = tuple_to_list(#db_entity{}),
    EFields = record_info(fields, db_entity),
    EKVs = lists:zip(EFields, EDefVals),

    [db_field | FDefVals] = tuple_to_list(#db_field{}),
    FFields = record_info(fields, db_field),
    FKVs = lists:zip(FFields, FDefVals),

    [db_link | LDefVals] = tuple_to_list(#db_link{}),
    LFields = record_info(fields, db_link),
    LKVs = lists:zip(LFields, LDefVals),

    NewESpecs = lists:map(
        fun(E) ->
            {fields, Fields} = lists:keyfind(fields, 1, E),
            E1 = lists:keystore(fields, 1, E, {fields, [proplist_to_record(db_field, F, FKVs) || F <- Fields]}),
            case lists:keyfind(deps, 1, E1) of
                false -> E1;
                {deps, Deps} ->
                    NewDeps = lists:map(fun(D) ->
                        {fields, DepFields} = lists:keyfind(fields, 1, D),
                        {type, DepType} = lists:keyfind(type, 1, D),
                        NewVia = case DepType of
                            many_to_many ->
                                {via, Via} = lists:keyfind(via, 1, D),
                                case Via of
                                    {_Table, _ExtraFields} ->
                                        Via;
                                    Table ->
                                        {Table, []}
                                end;
                            _ ->
                                undefined
                        end,
                        D1 = lists:keystore(via, 1, D, {via, NewVia}),
                        NewDepFields = lists:map(
                            fun
                                ({Side, {SrcField, DstField}}) when DepType =:= many_to_many,
                                (Side =:= src orelse Side =:= dst) ->
                                    {Side, {SrcField, DstField}};
                                ({Side, Field}) when DepType =:= many_to_many,
                                (Side =:= src orelse Side =:= dst) ->
                                    {Side, {Field, Field}};
                                ({SrcField, DstField}) ->
                                    {SrcField, DstField};
                                (Field) when is_list(Field) ->
                                    {Field, Field}
                            end, DepFields
                        ),
                        
                        NewD = lists:keystore(fields, 1, D1, {fields, NewDepFields}),
                        proplist_to_record(db_link, NewD, LKVs)
                    end, Deps),
                    lists:keystore(deps, 1, E1, {deps, NewDeps})
            end
        end, ESpecs
    ),

    RecESpecs = [ proplist_to_record(db_entity, S, EKVs) || S <- NewESpecs ],
    F_new_Str = string:join(["new(" ++ atom_to_list(EName) ++ ")->" ++ lists:flatten(io_lib:write(S))
        || S = #db_entity{name = EName} <- RecESpecs
    ], ";") ++ ".",

    F_supported_Str = string:join([
        "is_supported(\"" ++ Table ++ "\")->" ++ atom_to_list(EName) ++
        ";is_supported(" ++ atom_to_list(EName) ++ ")->" ++ atom_to_list(EName)
        || #db_entity{name = EName, table = Table} <- RecESpecs
    ], ";") ++ ";is_supported(_)->false.",
    

    compile_and_load(["-module(oakentity_hlp).", "-export([new/1, is_supported/1]).", F_new_Str, F_supported_Str]).
    
proplist_to_record(RAtom, S, DefaultKVs) when is_atom(RAtom) ->
    list_to_tuple([RAtom | [ get_recv(K, S, V) || {K, V} <- DefaultKVs ]]).


get_recv(F, L, DefVal) ->
    case lists:keyfind(F, 1, L) of
        false -> DefVal;
        {F, V} -> V
    end.
        

compile_and_load(Strings) -> 
    Forms = scan_and_parse(Strings, []), 
    {ok, Mod, Bin} = compile:forms(Forms), 
    {module, _M} = code:load_binary(oakentity_hlp, "oakentity_hlp", Bin), 
    code:purge(Mod),
    ok.


scan_and_parse([], Forms) -> 
    Res = lists:reverse(Forms), 
    Res;
scan_and_parse([S | Strings], Forms) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, Form} = erl_parse:parse_form(Tokens),
    scan_and_parse(Strings, [Form | Forms]). 
%% compile related function ^^^^^^^^^^

