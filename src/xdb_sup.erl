%%%-------------------------------------------------------------------
%%% @doc
%%% Top level supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.
