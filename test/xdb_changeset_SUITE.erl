-module(xdb_changeset_SUITE).

-behaviour(ct_suite).

%% Common Test
-export([all/0]).
%% Test Cases
-export([t_changeset/1, t_add_error/1, t_cast/1, t_change/1, t_put_change/1,
         t_get_change/1, t_delete_change/1, t_apply_changes/1, t_validate_change/1,
         t_validate_required/1, t_validate_inclusion/1, t_validate_number/1, t_validate_length/1,
         t_validate_format/1, t_nested_changeset_validations/1]).

-include_lib("common_test/include/ct.hrl").

-import(xdb_ct, [assert_error/2]).

-define(EXCLUDED_FUNS, [module_info, all]).
-define(PERMITTED,
        [id, first_name, last_name, age, address, height, description, status, birthdate]).
-define(REQUIRED, [id, first_name, last_name, age]).
-define(PERSON,
        #{id => 1,
          last_name => <<"other">>,
          age => 33,
          height => 1.85,
          birthdate => <<"1980-09-22">>,
          created_at => {{2012, 2, 16}, {1, 6, 48}},
          is_blocked => false,
          status => "active"}).

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec t_changeset(xdb_ct:config()) -> ok.
t_changeset(_Config) ->
    CS = xdb_changeset:change(
             person:schema(default_person()), #{}),
    person = xdb_changeset:schema(CS),
    #{'__meta__' := #{schema := person}} = xdb_changeset:data(CS),
    undefined = xdb_changeset:params(CS),
    [] = xdb_changeset:errors(CS),
    #{} = xdb_changeset:changes(CS),
    true = xdb_changeset:is_valid(CS),
    #{id := integer} = xdb_changeset:types(CS),
    [] = xdb_changeset:required(CS),
    undefined = xdb_changeset:repo(CS),
    undefined = xdb_changeset:action(CS),
    #{} = xdb_changeset:filters(CS),
    ok.

-spec t_add_error(xdb_ct:config()) -> ok.
t_add_error(_Config) ->
    %% run changeset pipeline adding an error
    InitChangeset = person:schema(default_person()),
    UpdatedChangeset = xdb_changeset:cast(InitChangeset, ?PERSON, ?PERMITTED),
    CS = xdb_changeset:add_error(UpdatedChangeset, status, <<"Invalid">>),
    %% validate errors
    false = xdb_changeset:is_valid(CS),
    1 = length(xdb_changeset:errors(CS)),
    _ = validate_cs_errors(CS, [status]),
    ok.

-spec t_cast(xdb_ct:config()) -> ok.
t_cast(_Config) ->
    %% create a person doc
    Person = default_person(),
    PersonSchema = person:schema(Person),

    %% create params to be cast adding some intentional errors
    Params = ?PERSON#{age => '33', missing => 1},
    Permitted = [missing | ?PERMITTED],

    %% run changeset pipeline
    ExpectedChanges =
        #{birthdate => {{1980, 9, 22}, {0, 0, 0}},
          height => 1.85,
          id => 1,
          last_name => <<"other">>,
          status => <<"active">>},
    InitCS = xdb_changeset:cast(PersonSchema, Params, Permitted),
    CS = validate_cs(InitCS,
                     #{schema => person,
                       data => PersonSchema,
                       params => maps:with(Permitted, Params),
                       changes => ExpectedChanges,
                       types => {true, fun(M) -> maps:size(M) > 0 end},
                       required => {[], fun(L) -> L end}}),

    %% validate errors
    false = xdb_changeset:is_valid(CS),
    1 = length(xdb_changeset:errors(CS)),
    _ = validate_cs_errors(CS, [age]),

    PersonCS = xdb_changeset:cast(PersonSchema, #{}, ?PERMITTED),
    CS1 = xdb_changeset:cast(PersonCS, #{last_name => <<"other">>}, Permitted),

    %% validate errors
    true = xdb_changeset:is_valid(CS1),
    0 = length(xdb_changeset:errors(CS1)),
    ok.

-spec t_change(xdb_ct:config()) -> ok.
t_change(_Config) ->
    InitCS1 = person:schema(default_person()),
    UpdatedCS1 = xdb_changeset:cast(InitCS1, #{}, ?PERMITTED),
    CS1 = xdb_changeset:change(UpdatedCS1, #{last_name => <<"other">>}),

    1 =
        maps:size(
            xdb_changeset:changes(CS1)),

    InitCS2 = person:schema(default_person()),
    UpdatedCS2 = xdb_changeset:change(InitCS2, #{last_name => <<"Darwin">>}),
    CS2 = xdb_changeset:change(UpdatedCS2, #{last_name => <<"other">>}),

    1 =
        maps:size(
            xdb_changeset:changes(CS2)),

    InitCS3 = person:schema(default_person()),
    UpdatedCS3 = xdb_changeset:change(InitCS3, #{last_name => <<"Darwin">>}),
    CS3 = xdb_changeset:cast(UpdatedCS3, #{}, ?PERMITTED),

    1 =
        maps:size(
            xdb_changeset:changes(CS3)),
    ok.

-spec t_put_change(xdb_ct:config()) -> ok.
t_put_change(_Config) ->
    InitCS1 = person:schema(default_person()),
    UpdatedCS1 = xdb_changeset:cast(InitCS1, #{}, ?PERMITTED),
    CS1 = xdb_changeset:put_change(UpdatedCS1, last_name, <<"other">>),
    #{last_name := <<"other">>} = xdb_changeset:changes(CS1),

    InitCS2 = person:schema(default_person()),
    UpdatedCS2 = xdb_changeset:cast(InitCS2, #{}, ?PERMITTED),
    CS2 = xdb_changeset:put_change(UpdatedCS2, last_name, <<"Doe">>),
    0 =
        maps:size(
            xdb_changeset:changes(CS2)),

    InitCS3 = person:schema(default_person()),
    UpdatedCS3 = xdb_changeset:cast(InitCS3, #{}, ?PERMITTED),
    Updated1CS3 = xdb_changeset:put_change(UpdatedCS3, first_name, <<"other">>),
    Updated2CS3 = xdb_changeset:put_change(Updated1CS3, last_name, <<"other">>),
    CS3 = xdb_changeset:put_change(Updated2CS3, weird_field1, undefined),
    2 =
        maps:size(
            xdb_changeset:changes(CS3)),

    InitCS4 =
        person:schema(
            person:new(<<"other">>, <<"other">>)),
    UpdatedCS4 = xdb_changeset:cast(InitCS4, #{}, ?PERMITTED),
    Updated1CS4 = xdb_changeset:put_change(UpdatedCS4, last_name, <<"other">>),
    Updated2CS4 = xdb_changeset:cast(Updated1CS4, #{last_name => <<"other">>}, ?PERMITTED),
    CS4 = xdb_changeset:put_change(Updated2CS4, last_name, <<"other">>),
    0 =
        maps:size(
            xdb_changeset:changes(CS4)),
    ok.

-spec t_get_change(xdb_ct:config()) -> ok.
t_get_change(_Config) ->
    InitCS = person:schema(default_person()),
    UpdatedCS = xdb_changeset:cast(InitCS, #{}, ?PERMITTED),
    CS1 = xdb_changeset:put_change(UpdatedCS, last_name, <<"other">>),

    1 =
        maps:size(
            xdb_changeset:changes(CS1)),

    <<"other">> = xdb_changeset:get_change(CS1, last_name),
    undefined = xdb_changeset:get_change(CS1, first_name),
    <<"default">> = xdb_changeset:get_change(CS1, first_name, <<"default">>),

    ok.

-spec t_delete_change(xdb_ct:config()) -> ok.
t_delete_change(_Config) ->
    InitCS = person:schema(default_person()),
    UpdatedCS = xdb_changeset:cast(InitCS, #{}, ?PERMITTED),
    CS1 = xdb_changeset:put_change(UpdatedCS, last_name, <<"other">>),

    1 =
        maps:size(
            xdb_changeset:changes(CS1)),

    CS2 = xdb_changeset:delete_change(CS1, last_name),
    0 =
        maps:size(
            xdb_changeset:changes(CS2)),

    ok.

-spec t_apply_changes(xdb_ct:config()) -> ok.
t_apply_changes(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% run changeset pipeline
    CS1 = xdb_changeset:cast(Person, ?PERSON#{missing => 1}, ?PERMITTED),
    Data = xdb_changeset:data(CS1),
    true = Data == Person,
    undefined = person:id(Person),
    <<"Doe">> = person:last_name(Person),
    undefined = person:age(Person),

    %% apply changes
    NewPerson = xdb_changeset:apply_changes(CS1),
    false = NewPerson == Person,
    1 = person:id(NewPerson),
    <<"other">> = person:last_name(NewPerson),
    33 = person:age(NewPerson),
    6 =
        maps:size(
            xdb_changeset:changes(CS1)),

    %% run changeset pipeline
    CS2 = xdb_changeset:cast(Person, #{}, ?PERMITTED),
    0 =
        maps:size(
            xdb_changeset:changes(CS2)),
    Person = xdb_changeset:apply_changes(CS2),

    %% run changeset pipeline
    InitCS3 = xdb_changeset:cast(Person, #{}, ?PERMITTED),
    CS3 = xdb_changeset:put_change(InitCS3, missing, 2),

    0 =
        maps:size(
            xdb_changeset:changes(CS3)),
    Person = xdb_changeset:apply_changes(CS3),

    ok.

-spec t_validate_change(xdb_ct:config()) -> ok.
t_validate_change(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% run changeset pipeline
    InitCS1 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    CS1 = xdb_changeset:validate_change(InitCS1,
                                        age,
                                        fun(age, Age) ->
                                           case Age > 30 of
                                               true ->
                                                   [{age, <<"cannot be greater than 30">>}];
                                               false ->
                                                   []
                                           end
                                        end),

    %% validate errors
    false = xdb_changeset:is_valid(CS1),
    [{age, {<<"cannot be greater than 30">>, []}}] = xdb_changeset:errors(CS1),

    ok.

-spec t_validate_required(xdb_ct:config()) -> ok.
t_validate_required(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% run changeset pipeline
    InitCS1 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    CS1 = xdb_changeset:validate_required(InitCS1, ?REQUIRED),

    %% validate errors
    true = xdb_changeset:is_valid(CS1),
    0 = length(xdb_changeset:errors(CS1)),

    %% run changeset pipeline
    InitCS2 = xdb_changeset:cast(Person, ?PERSON#{age => nil}, ?PERMITTED),
    CS2 = xdb_changeset:validate_required(InitCS2, [address | ?REQUIRED]),

    %% validate errors
    false = xdb_changeset:is_valid(CS2),
    2 = length(xdb_changeset:errors(CS2)),
    _ = validate_cs_errors(CS2, [address, age]),

    %% should fails
    assert_error(fun() ->
                    Ch = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
                    xdb_changeset:validate_required(Ch, [invalid | ?REQUIRED])
                 end,
                 {badarg, invalid}).

-spec t_validate_inclusion(xdb_ct:config()) -> ok.
t_validate_inclusion(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% valid statuses
    Statuses = [<<"active">>, <<"blocked">>],

    %% run changeset pipeline
    InitCS1 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    UpdatedCS1 = xdb_changeset:validate_required(InitCS1, ?REQUIRED),
    CS1 = xdb_changeset:validate_inclusion(UpdatedCS1, status, Statuses),

    %% validate errors
    true = xdb_changeset:is_valid(CS1),
    0 = length(xdb_changeset:errors(CS1)),

    %% run changeset pipeline
    InitCS2 = xdb_changeset:cast(Person, ?PERSON#{status => <<"invalid">>}, ?PERMITTED),
    UpdatedCS2 = xdb_changeset:validate_required(InitCS2, ?REQUIRED),
    CS2 = xdb_changeset:validate_inclusion(UpdatedCS2, status, Statuses),

    %% validate errors
    false = xdb_changeset:is_valid(CS2),
    1 = length(xdb_changeset:errors(CS2)),
    _ = validate_cs_errors(CS2, [status]),

    ok.

-spec t_validate_number(xdb_ct:config()) -> ok.
t_validate_number(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% run changeset pipeline
    InitCS1 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    CS1 = xdb_changeset:validate_number(InitCS1,
                                        age,
                                        [{less_than, 34},
                                         {less_than_or_equal_to, 33},
                                         {greater_than, 32},
                                         {greater_than_or_equal_to, 33},
                                         {equal_to, 33}]),

    %% validate errors
    true = xdb_changeset:is_valid(CS1),
    0 = length(xdb_changeset:errors(CS1)),

    ValidationSet =
        [[{less_than, 30}],
         [{less_than_or_equal_to, 30}],
         [{greater_than, 40}],
         [{greater_than_or_equal_to, 40}],
         [{equal_to, 30}],
         [{less_than, 30}, {equal_to, 30}]],

    ok =
        lists:foreach(fun(Validations) ->
                         %% run changeset pipeline
                         InitCS = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
                         CS = xdb_changeset:validate_number(InitCS, age, Validations),

                         %% validate errors
                         false = xdb_changeset:is_valid(CS),
                         1 = length(xdb_changeset:errors(CS)),
                         _ = validate_cs_errors(CS, [age])
                      end,
                      ValidationSet),

    %% should fails
    assert_error(fun() ->
                    InitCS2 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
                    xdb_changeset:validate_number(InitCS2, age, [{invalid_validation, 33}])
                 end,
                 {badarg, invalid_validation}).

-spec t_validate_length(xdb_ct:config()) -> ok.
t_validate_length(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% run changeset pipeline
    InitCS1 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    CS1 = xdb_changeset:validate_length(InitCS1, last_name, [{is, 5}, {min, 2}, {max, 10}]),

    %% validate errors
    true = xdb_changeset:is_valid(CS1),
    0 = length(xdb_changeset:errors(CS1)),

    ValidationSet = [[{is, 3}], [{min, 10}], [{max, 3}], [{is, 5}, {min, 2}, {max, 3}]],
    ok =
        lists:foreach(fun(Validations) ->
                         %% run changeset pipeline
                         InitCS = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
                         CS = xdb_changeset:validate_length(InitCS, last_name, Validations),

                         %% validate errors
                         false = xdb_changeset:is_valid(CS),
                         [{last_name, {_, [{validation, length}]}}] = xdb_changeset:errors(CS)
                      end,
                      ValidationSet),

    ok.

-spec t_validate_format(xdb_ct:config()) -> ok.
t_validate_format(_Config) ->
    %% create a person doc
    Person = person:schema(default_person()),

    %% run changeset pipeline
    InitCS1 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    ValidatedCS1 = xdb_changeset:validate_required(InitCS1, ?REQUIRED),
    CS1 = xdb_changeset:validate_format(ValidatedCS1, last_name, <<"^oth">>),

    %% validate errors
    true = xdb_changeset:is_valid(CS1),
    0 = length(xdb_changeset:errors(CS1)),

    %% run changeset pipeline
    InitCS2 = xdb_changeset:cast(Person, ?PERSON, ?PERMITTED),
    ValidatedCS2 = xdb_changeset:validate_required(InitCS2, ?REQUIRED),
    CS2 = xdb_changeset:validate_format(ValidatedCS2, last_name, <<"^Doe">>),

    %% validate errors
    false = xdb_changeset:is_valid(CS2),
    [{last_name, {<<"has invalid format">>, [{validation, format}]}}] =
        xdb_changeset:errors(CS2),

    ok.

-spec t_nested_changeset_validations(xdb_ct:config()) -> ok.
t_nested_changeset_validations(_Config) ->
    Person = person:new(<<"John">>, <<"Doe">>),
    Params =
        #{<<"age">> => 33,
          id => 1,
          <<"last_name">> => <<"other">>},
    _ = person:changeset(
            person:schema(Person), Params),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
default_person() ->
    person:new(<<"John">>, <<"Doe">>).

%% @private
validate_cs(CS, ParamsToCheck) ->
    maps:fold(fun (K, {Expected, Fun}, Acc) when is_function(Fun) ->
                      Expected = Fun(xdb_changeset:K(CS)),
                      Acc;
                  (K, V, Acc) ->
                      V = xdb_changeset:K(CS),
                      Acc
              end,
              CS,
              ParamsToCheck).

%% @private
validate_cs_errors(CS, ErrorKeys) ->
    Errors = xdb_changeset:errors(CS),
    [true = lists:keymember(K, 1, Errors) || K <- ErrorKeys].
