%%%-------------------------------------------------------------------
%% @doc strategy public API
%% @end
%%%-------------------------------------------------------------------

-module(strategy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_ranch(),
    start_epgsql_pool(),
    strategy_sup:start_link().
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

start_ranch()->
  {ok,Port} = application:get_env(strategy,port),
  {ok, _} = ranch:start_listener(strategy_game_tcp_endpoint,
	ranch_tcp, [{port, Port}],
	game_protocol, []
).

start_epgsql_pool()->
  Params = #{host => "localhost",
           port => 5432,
           username => "strategydb_user",
           password => "qwe",
           database => "strategydb"},
{ok, _} = epgsql_pool:start(main_pool, 10, 20, Params).
%%====================================================================
%% Internal functions
%%====================================================================
