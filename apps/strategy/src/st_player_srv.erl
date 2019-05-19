-module(st_player_srv).
-behavior(gen_server).

-export([start_link/1,auth/3,reg/3,start_game/1,stop/1,up/1,down/1,left/1,right/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("st_player.hrl").

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

-record(state,{
          socket :: pid(),
          battle_room :: pid(),
          player :: #player{}
}).

auth(PlayerSrv,Login,Pass)->
  gen_server:call(PlayerSrv,{auth,Login,Pass}).

reg(PlayerSrv,Login,Pass)->
  gen_server:call(PlayerSrv,{reg,Login,Pass}).

start_game(PlayerSrv)->
  gen_server:call(PlayerSrv,start_game).

stop(Pid)->
  gen_server:call(Pid,stop).

up(PlayerSrv)->
  gen_server:call(PlayerSrv,up).

down(PlayerSrv)->
  gen_server:call(PlayerSrv,down).

left(PlayerSrv)->
  gen_server:call(PlayerSrv,left).

right(PlayerSrv)->
  gen_server:call(PlayerSrv,right).

%%% gen_server API

init(Socket) ->
    State = #state{ socket = Socket, player = #player{}},
    lager:info("player with state PID: ~p, State :  ~p",[self(),State]),
    {ok, State}.


handle_call(stop,_From,State)->
  {stop,normal,ok,State};

handle_call({reg,Login,Pass},_From,State)->
  {ok,_,Answer} = epgsql_pool:query(main_pool,"SELECT * FROM player where nickname = $1",[Login]),
  case Answer of
    [] -> epgsql_pool:query(main_pool,"INSERT INTO player (nickname,password) VALUES ($1,$2)",[Login,Pass]),
          {reply,ok,State};
    _ ->  {reply,error,State}
  end;

handle_call({auth,Login,Pass},_From,State = #state{player = Player})->
  {ok,_,Answer} = epgsql_pool:query(main_pool,"SELECT * FROM player where nickname = $1 AND password= $2",[Login,Pass]),
  case Answer of
    [] -> {reply,error,State};
    [{Id,_,_,_,_,_,_}]->  Token = list_to_binary(lists:flatten(io_lib:format("~p",[make_ref()]))),
                          epgsql_pool:query(main_pool,"INSERT INTO auth_token (player_id,token,expired_at)
                                                       VALUES ($1,$2,now() + interval '2 hour')",[Id,Token]),
                          {reply,ok,State#state{player = Player#player{token = Token, id = Id}}}
  end;

handle_call(start_game,_From,State = #state{ socket = Socket, player = Player})->
  #player{token = Token, id = Id} = Player,
  %%lager:info("player with Token: ~p, Id :  ~p",[Token,Id]),
  {ok,_,Answer} = epgsql_pool:query(main_pool,"SELECT * FROM auth_token
                                               where token = $1 AND player_id= $2 AND expired_at > now()",[Token,Id]),
  case Answer of
    [] -> {reply,error,State};
    [_] -> case st_player_mm:add_player(Socket,self(),Id) of
          ok -> {reply,ok,State};
          started -> {reply,started,State}
        end

  end;

handle_call(up, _From, State = #state{battle_room = Battle_Room}) ->
  st_battle_room:up(self(),Battle_Room),
  {reply, ok, State};

handle_call(down, _From, State = #state{battle_room = Battle_Room}) ->
  st_battle_room:down(self(),Battle_Room),
  {reply, ok, State};

handle_call(left, _From, State = #state{battle_room = Battle_Room}) ->
  st_battle_room:left(self(),Battle_Room),
  {reply, ok, State};

handle_call(right, _From, State = #state{battle_room = Battle_Room}) ->
  st_battle_room:right(self(),Battle_Room),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, #player{} = State) ->
    {noreply, State}.

handle_info({battle_room,Battle_Room}, State) ->
    {noreply, State#state{battle_room = Battle_Room}};

handle_info(close_room, State) ->
    {noreply, State#state{battle_room = undefined}};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
{ok, State}.
