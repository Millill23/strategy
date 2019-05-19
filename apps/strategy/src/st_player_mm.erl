-module(st_player_mm).
-behavior(gen_server).

-export([start_link/0,add_player/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, no_args, []).

add_player(Socket,PlayerSrv,PlayerId)->
  gen_server:call(?MODULE,{add_player,Socket,PlayerSrv,PlayerId}).

%%% gen_server API

init(no_args) ->
    lager:info("~p init",[?MODULE]),
    State = [],
    {ok, State}.

handle_call({add_player,Socket,PlayerSrv,PlayerId}, _From, State) ->
    case State of
      []-> State2 = [{Socket,PlayerSrv,PlayerId}],
           {reply, ok, State2};
      [{Socket2,PlayerSrv2,PlayerId2}]-> {ok,Battle_Room} = supervisor:start_child(st_battle_rooms_sup,[{PlayerSrv,Socket,PlayerId},{PlayerSrv2,Socket2,PlayerId2}]),
                               Battle_Map = st_battle_room:show_battle_map(Battle_Room),
                               gen_tcp:send(Socket2,<<"GAME STARTED!\n",Battle_Map/binary>>),
                               gen_tcp:send(Socket,<<"GAME STARTED!\n",Battle_Map/binary>>),
                               State2 = [],
                               PlayerSrv ! {battle_room,Battle_Room},
                               PlayerSrv2 ! {battle_room,Battle_Room},
                               {reply,started,State2}
    end.

handle_cast(_Request,State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
{ok, State}.
