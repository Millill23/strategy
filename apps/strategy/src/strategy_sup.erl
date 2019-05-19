%%%-------------------------------------------------------------------
%% @doc strategy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(strategy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => st_player_sup,
           start => {st_player_sup, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => supervisor,
           modules => [st_player_sup]},
           #{id => st_battle_rooms_sup,
             start => {st_battle_rooms_sup,start_link,[]},
             restart => permanent,
             shutdown => 2000,
             type => supervisor,
             modules => [st_battle_rooms_sup]
              },
         #{id => st_player_mm,
           start => {st_player_mm,start_link,[]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [st_player_mm]
            }
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
%%====================================================================
%% Internal functions
%%====================================================================
