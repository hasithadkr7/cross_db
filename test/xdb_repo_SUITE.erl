-module(xdb_repo_SUITE).

-behaviour(ct_suite).

%% Common Test
-export([all/0, init_per_suite/1, end_per_suite/1]).
%% Test Cases
-export([t_insert/1, t_insert_all/1, t_update/1, t_delete/1, t_all/1, t_all_by/1, t_get/1,
         t_get_by/1]).

-import(xdb_ct, [assert_error/2]).

-define(EXCLUDED_FUNS, [module_info, all, init_per_suite, end_per_suite]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(xdb_ct:config()) -> xdb_ct:config().
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cross_db),
    Config.

-spec end_per_suite(xdb_ct:config()) -> ok.
end_per_suite(_) ->
    ok = application:stop(cross_db).

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_insert(xdb_ct:config()) -> ok.
t_insert(_Config) ->
    {ok, #{id := 1}} =
        xdb_test_repo:insert(
            person:schema(#{id => 1})),
    InitCS1 = person:schema(#{id => 1}),
    CS1 = xdb_changeset:change(InitCS1, #{first_name => <<"Joe">>}),
    {ok, #{id := 1}} = xdb_test_repo:insert(CS1),

    InitCS = person:schema(#{id => 1}),
    UpdatedCS1 = xdb_changeset:change(InitCS, #{first_name => <<"Joe">>}),
    ErCS1 = xdb_changeset:add_error(UpdatedCS1, first_name, <<"Invalid">>),
    {error, CS} = xdb_test_repo:insert(ErCS1),

    {error, [{constraint, invalid_key}]} =
        xdb_test_repo:insert(
            person:schema(#{id => -2})),

    ok = assert_error(fun() -> xdb_test_repo:update(CS) end, badarg),

    ok =
        assert_error(fun() ->
                        xdb_test_repo:insert(
                            person:schema(#{id => -1}))
                     end,
                     my_error),

    ok =
        assert_error(fun() ->
                        xdb_test_repo:insert(
                            person:schema(#{id => -11}))
                     end,
                     badarg).

-spec t_insert_all(xdb_ct:config()) -> ok.
t_insert_all(_Config) ->
    {1, [#{id := 1}]} = xdb_test_repo:insert_all(person, [#{id => 1}]),
    {2, [#{id := 1}, #{id := 2}]} =
        xdb_test_repo:insert_all(person, [#{id => 1}, #{id => 2}]),
    ok.

-spec t_update(xdb_ct:config()) -> ok.
t_update(_Config) ->
    ok =
        assert_error(fun() ->
                        xdb_test_repo:update(
                            person:schema(#{id => 1}))
                     end,
                     function_clause),

    {ok, #{id := 1}} =
        xdb_test_repo:update(
            xdb_changeset:change(
                person:schema(#{id => 1}), #{first_name => <<"Joe">>})),

    {error, #{}} =
        xdb_test_repo:update(
            xdb_changeset:add_error(
                xdb_changeset:change(
                    person:schema(#{id => 1}), #{first_name => <<"Joe">>}),
                first_name,
                <<"Invalid">>)),

    ok =
        assert_error(fun() ->
                        xdb_test_repo:update(
                            xdb_changeset:change(
                                person:schema(#{id => -1}), #{id => -2}))
                     end,
                     stale_entry_error).

-spec t_delete(xdb_ct:config()) -> ok.
t_delete(_Config) ->
    {ok, #{id := 1}} =
        xdb_test_repo:delete(
            person:schema(#{id => 1})),

    CS = person:schema(#{id => 1}),
    UpdatedCS = xdb_changeset:change(CS, #{first_name => <<"Joe">>}),
    {ok, #{id := 1}} = xdb_test_repo:delete(UpdatedCS),

    ErrCS =
        xdb_changeset:change(
            person:schema(#{id => 1}), #{first_name => <<"Joe">>}),
    UpdatedErrCS = xdb_changeset:add_error(ErrCS, first_name, <<"Invalid">>),
    {error, #{}} = xdb_test_repo:delete(UpdatedErrCS),

    ok =
        assert_error(fun() ->
                        xdb_test_repo:delete(
                            person:schema(#{id => -1}))
                     end,
                     stale_entry_error),

    ok =
        assert_error(fun() ->
                        xdb_test_repo:delete(
                            person:schema(#{}))
                     end,
                     no_primary_key_value_error).

-spec t_all(xdb_ct:config()) -> ok.
t_all(_Config) ->
    [#{'__meta__' := _, id := 1}] = xdb_test_repo:all(person),

    Query = xdb_query:from(person, [{where, [{id, 1}]}]),
    [#{'__meta__' := _, id := 1}] = xdb_test_repo:all(Query),
    ok.

-spec t_get(xdb_ct:config()) -> ok.
t_get(_Config) ->
    #{'__meta__' := _, id := 1} = xdb_test_repo:get(person, 1),
    undefined = xdb_test_repo:get(person, -1),

    ok = assert_error(fun() -> xdb_test_repo:get(person, -11) end, multiple_results_error),

    ok = assert_error(fun() -> xdb_test_repo:get(account, 1) end, badarg).

-spec t_get_by(xdb_ct:config()) -> ok.
t_get_by(_Config) ->
    #{'__meta__' := _, id := 1} = xdb_test_repo:get_by(person, [{id, 1}]),
    ok.

-spec t_all_by(xdb_ct:config()) -> ok.
t_all_by(_Config) ->
    [#{'__meta__' := _, id := 1}] = xdb_test_repo:all_by(person, [{id, 1}]),
    ok.
