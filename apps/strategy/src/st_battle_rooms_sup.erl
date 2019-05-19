%%%-------------------------------------------------------------------
%% @doc strategy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(st_battle_rooms_sup).

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
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => st_battle_room,
           start => {st_battle_room, start_link, []},
           restart => temporary,
           shutdown => 2000,
           type => worker,
           modules => [st_battle_room]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
%%====================================================================
%% Internal functions
%%====================================================================
