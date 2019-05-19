-module(st_battle_room).
-behavior(gen_server).

-export([start_link/2,switch_curr_player/1,check_curr_player/2,check_have_move/2,send_to_players/2,send_error_to_player/2,up/2,down/2,left/2,right/2,show_battle_map/1,endgame/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link({Pid1,Socket1,PlayerId1},{Pid2,Socket2,PlayerId2}) ->
    gen_server:start_link(?MODULE, [{Pid1,Socket1,PlayerId1},{Pid2,Socket2,PlayerId2}], []).

switch_curr_player(BattlePid)->
    gen_server:call(BattlePid,switch_curr_player).

check_curr_player(PlayerPid,BattlePid)->
  gen_server:call(BattlePid,{check_curr_player,PlayerPid}).

check_have_move(BattlePid,PlayerPid) ->
    Reply = gen_server:call(BattlePid,{check_have_move,PlayerPid}),
    case Reply of
      ok -> ok;
      {winner,a} -> endgame(BattlePid,{<<"PLAYER A WINNER!\r\n">>,a});
      {winner,b} -> endgame(BattlePid,{<<"PLAYER B WINNER!\r\n">>,b})
    end.

send_to_players(BattlePid,Msg)->
    gen_server:call(BattlePid,{send_to_players,Msg}).

send_error_to_player(BattlePid,PlayerPid) ->
  gen_server:call(BattlePid,{send_error_to_player,PlayerPid}).

up(PlayerPid,BattlePid)->
  Reply_From_Check = check_curr_player(PlayerPid,BattlePid),
  case Reply_From_Check of
    ok->
          Reply = gen_server:call(BattlePid,{up,PlayerPid}),
          case Reply of
            ok -> switch_curr_player(BattlePid),
                Battle_Map = show_battle_map(BattlePid),
                send_to_players(BattlePid,Battle_Map),
                check_have_move(BattlePid,PlayerPid);
            error -> send_error_to_player(BattlePid,PlayerPid)
          end;
    error -> ok
  end.

down(PlayerPid,BattlePid)->
  Reply_From_Check = check_curr_player(PlayerPid,BattlePid),
  case Reply_From_Check of
    ok->
          Reply = gen_server:call(BattlePid,{down,PlayerPid}),
          case Reply of
            ok -> switch_curr_player(BattlePid),
                Battle_Map = show_battle_map(BattlePid),
                send_to_players(BattlePid,Battle_Map),
                check_have_move(BattlePid,PlayerPid);
            error -> send_error_to_player(BattlePid,PlayerPid)
          end;
    error -> ok
  end.

left(PlayerPid,BattlePid)->
  Reply_From_Check = check_curr_player(PlayerPid,BattlePid),
  case Reply_From_Check of
    ok->
          Reply = gen_server:call(BattlePid,{left,PlayerPid}),
          case Reply of
            ok -> switch_curr_player(BattlePid),
                Battle_Map = show_battle_map(BattlePid),
                send_to_players(BattlePid,Battle_Map),
                check_have_move(BattlePid,PlayerPid);
            error -> send_error_to_player(BattlePid,PlayerPid)
          end;
    error -> ok
  end.

right(PlayerPid,BattlePid)->
  Reply_From_Check = check_curr_player(PlayerPid,BattlePid),
  case Reply_From_Check of
    ok->
          Reply = gen_server:call(BattlePid,{right,PlayerPid}),
          case Reply of
            ok -> switch_curr_player(BattlePid),
                Battle_Map = show_battle_map(BattlePid),
                send_to_players(BattlePid,Battle_Map),
                check_have_move(BattlePid,PlayerPid);
            error -> send_error_to_player(BattlePid,PlayerPid)
          end;
    error -> ok
  end.
show_battle_map(BattlePid)->
  gen_server:call(BattlePid,show_battle_map).

endgame(BattlePid,Winner)->
  gen_server:call(BattlePid,{endgame,Winner}).
%%% gen_server API

init([{Pid1,Socket1,PlayerId1},{Pid2,Socket2,PlayerId2}]) ->
    {ok,{X,Y}} = application:get_env(strategy, field_size),
    Field_Size = X * Y,
    List = lists:seq(1,Field_Size),
    Last = lists:last(List),
    F = fun(Num,Acc) -> Acc#{Num => <<" [ ] ">>} end,
    State = lists:foldl(F,#{},List),
    State2 = State#{Last := <<" [B] ">>, b => Last,1 => <<" [A] ">>, a => 1,
                    Pid1 => p1, Pid2 => p2, s1 => Socket1, s2 => Socket2,
                    curr_player => p1, p1 => Pid1, p2 => Pid2,playerId1 => PlayerId1, playerId2 => PlayerId2},
    {ok, State2}.

handle_call({up,Pid}, _From, State) ->
  {ok,{X,_}} = application:get_env(strategy, field_size),
    {Curr,Player} = case maps:get(Pid,State) of
      p1 -> {maps:get(a,State),{<<" [A] ">>,a}};
      p2 -> {maps:get(b,State),{<<" [B] ">>,b}}
      end,
    Move = Curr -X,
    {reply,Reply,NewState} = user_move(State,Move,Curr,Player),
    {reply,Reply,NewState};

handle_call({down,Pid}, _From, State) ->
  {ok,{X,_}} = application:get_env(strategy, field_size),
  {Curr,Player} = case maps:get(Pid,State) of
    p1 -> {maps:get(a,State),{<<" [A] ">>,a}};
    p2 -> {maps:get(b,State),{<<" [B] ">>,b}}
    end,
  Move = Curr + X,
  {reply,Reply,NewState} = user_move(State,Move,Curr,Player),
  {reply,Reply,NewState};

handle_call({left,Pid}, _From, State) ->
  {ok,{X,_}} = application:get_env(strategy, field_size),
  {Curr,Player} = case maps:get(Pid,State) of
    p1 -> {maps:get(a,State),{<<" [A] ">>,a}};
    p2 -> {maps:get(b,State),{<<" [B] ">>,b}}
    end,
  Move = Curr - 1,
  case Move rem X ==0 of
    true -> {reply,error,State};
    false ->
            {reply,Reply,NewState} = user_move(State,Move,Curr,Player),
            {reply,Reply,NewState}
  end;

handle_call({right,Pid}, _From, State) ->
  {ok,{X,_}} = application:get_env(strategy, field_size),
  {Curr,Player} = case maps:get(Pid,State) of
    p1 -> {maps:get(a,State),{<<" [A] ">>,a}};
    p2 -> {maps:get(b,State),{<<" [B] ">>,b}}
    end,
  case Curr rem X ==0 of
    true -> {reply,error,State};
    false ->
            Move = Curr + 1,
            {reply,Reply,NewState} = user_move(State,Move,Curr,Player),
            {reply,Reply,NewState}
  end;

handle_call(show_battle_map, _From, State) ->
    Reply = get_battle_map(State),
    {reply,Reply,State};

handle_call({send_to_players,Msg}, _From, State) ->
    Socket1 = maps:get(s1,State),
    Socket2 = maps:get(s2,State),
    gen_tcp:send(Socket1,<<Msg/binary>>),
    gen_tcp:send(Socket2,<<Msg/binary>>),
    {reply,ok,State};

handle_call({send_error_to_player,PlayerPid}, _From, State) ->
    Socket = case maps:get(PlayerPid,State) of
              p1-> maps:get(s1,State);
              p2-> maps:get(s2,State)
             end,
    gen_tcp:send(Socket,<<"ERROR, please try once more\r\n">>),
    {reply,ok,State};

handle_call(switch_curr_player, _From, State) ->
    case maps:get(curr_player,State) of
      p1 -> {reply,ok,State#{curr_player:= p2}};
      p2 -> {reply,ok,State#{curr_player:= p1}}
    end;

handle_call({check_curr_player,PlayerPid}, _From, State) ->
    Curr_Player = maps:get(curr_player,State),
    case maps:get(PlayerPid,State) of
      Curr_Player -> {reply,ok,State};
      _ -> Socket = case Curr_Player of
                        p1 -> maps:get(s2,State);
                        p2 -> maps:get(s1,State)
                    end,
            gen_tcp:send(Socket,<<"NOT YOUR MOVE! WAIT PLEASE!\r\n">>),
            {reply,error,State}
          end;

handle_call({check_have_move,Pid},_From,State)->
  {ok,{X,_}} = application:get_env(strategy, field_size),
  {Curr,{_,Player}} = case maps:get(Pid,State) of
    p1 -> {maps:get(a,State),{<<" [A] ">>,a}};
    p2 -> {maps:get(b,State),{<<" [B] ">>,b}}
    end,
    F1 = maps:find(Curr-X,State) == {ok,<<" [ ] ">>},
    F2 = maps:find(Curr+X,State) == {ok,<<" [ ] ">>},
    F3 =(maps:find(Curr-1,State) == {ok,<<" [ ] ">>}) and ((Curr-1 rem X) =/= 0),
    F4 = (maps:find(Curr+1,State) == {ok,<<" [ ] ">>}) and ((Curr rem X) =/= 0),
  if
    F1 -> {reply,ok,State};
    F2 -> {reply,ok,State};
    F3 -> {reply,ok,State};
    F4 -> {reply,ok,State};
    true -> case Player of
                        a-> {reply,{winner,b},State};
                        b-> {reply,{winner,a},State}
                     end
  end;

handle_call({endgame,{WinnerMsg,WinnerAtom}}, _From, State) ->
  lager:info("START ENDGAME!"),
  Socket1 = maps:get(s1,State),
  Socket2 = maps:get(s2,State),
  Pid1 = maps:get(p1,State),
  Pid2 = maps:get(p2,State),
  gen_tcp:send(Socket1,<<WinnerMsg/binary>>),
  gen_tcp:send(Socket2,<<WinnerMsg/binary>>),
    lager:info("1 WITH PID: ~p",[Pid1]),
  Id1 = maps:get(playerId1,State),
  Id2 = maps:get(playerId2,State),
  WinnerId = case WinnerAtom of
              a -> epgsql_pool:query(main_pool,"UPDATE player
                                                SET played_battles = played_battles +1,
                                                    battles_won = battles_won +1
                                                WHERE id = $1",[Id1]),
                   epgsql_pool:query(main_pool,"UPDATE player
                                                SET played_battles = played_battles +1
                                                WHERE id = $1",[Id2]),
                  Id1;
              b -> epgsql_pool:query(main_pool,"UPDATE player
                                                SET played_battles = played_battles +1,
                                                    battles_won = battles_won +1
                                                WHERE id = $1",[Id2]),
                   epgsql_pool:query(main_pool,"UPDATE player
                                                SET played_battles = played_battles +1
                                                WHERE id = $1",[Id1]),
                  Id2
            end,
  epgsql_pool:query(main_pool,"INSERT INTO battle (player_id_1,player_id_2,winner_id)
                               VALUES ($1,$2,$3)",[Id1,Id2,WinnerId]),
  Pid1 ! close_room,
  Pid2 ! close_room,
  {stop,normal,ok,State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
{ok, State}.

get_battle_map(State)->
  {ok,{X,Y}} = application:get_env(strategy, field_size),
  Field_Size = X*Y,
  L = lists:reverse(lists:seq(1,Field_Size)),
  get_battle_map(L,State,<<>>).

get_battle_map([],_State,Acc) -> <<"************************\r\n",Acc/binary>>;
get_battle_map([Num|Tail],State,Acc)->
  {ok,{X,_}} = application:get_env(strategy, field_size),
    Val = maps:get(Num,State),
    case Num rem X ==0 of
      true -> get_battle_map(Tail,State,<<Val/binary,"\r\n",Acc/binary>>);
      false -> get_battle_map(Tail,State,<<Val/binary,Acc/binary>>)
    end.

user_move(State,Move,Curr,Player)->
  {PlayerCell,PlayerPid} = Player,
  case maps:is_key(Move,State) of
    true -> case maps:get(Move,State) of
            <<" [ ] ">> -> NewState = State#{PlayerPid := Move, Move := <<PlayerCell/binary>>,Curr := <<" [X] ">>},
                           {reply,ok,NewState};
            _ -> {reply,error,State}
          end;
    false ->  {reply,error,State}
  end.
