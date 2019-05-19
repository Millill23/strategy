-module(game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

-record(state,{
      transport,
      socket,
      player_srv

}).

start_link(Ref, _Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, Socket} = ranch:handshake(Ref),
  {ok,PlayerSrv} = supervisor:start_child(st_player_sup,[Socket]),
  gen_tcp:controlling_process(Socket, PlayerSrv),
  State = #state{transport = Transport,socket = Socket,player_srv = PlayerSrv},
  loop(State).

loop(#state{socket = Socket, transport = Transport, player_srv = PlayerSrv} = State) ->
  {ok,ClientTimeOutMin} = application:get_env(strategy,client_disconnect_timeout),
  ClientTimeOutMSec = ClientTimeOutMin * 60 * 1000,
	case Transport:recv(Socket, 0, ClientTimeOutMSec) of
    {ok,<<"PING\r\n">>} ->
        Reply = handle_ping(),
        Transport:send(Socket,Reply),
        loop(State);
    {ok,<<"AUTH: ",LoginPass/binary>>} ->
        Reply = handle_auth(LoginPass,PlayerSrv),
        case Reply of
          {ok,R} -> Transport:send(Socket,R),
                    loop(State);
          Error ->  Transport:send(Socket,Error),
                    loop(State)
        end;
    {ok,<<"REG: ",LoginPass/binary>>} ->
        Reply = handle_reg(LoginPass,PlayerSrv),
        case Reply of
          {ok,R} -> Transport:send(Socket,R),
                    loop(State);
          Error ->  Transport:send(Socket,Error),
                    loop(State)
        end;
    {ok,<<"START\r\n">>} ->
        Reply = handle_game(PlayerSrv),
        case Reply of
          {ok,R} -> Transport:send(Socket,R),
                    loop(State);
          started -> loop(State);
          Error -> Transport:send(Socket,Error),
                   loop(State)
          end;

    {ok,<<"UP\r\n">>} ->
        handle_up(PlayerSrv),
        loop(State);

    {ok,<<"DOWN\r\n">>} ->
      handle_down(PlayerSrv),
      loop(State);

    {ok,<<"LEFT\r\n">>} ->
      handle_left(PlayerSrv),
      loop(State);

    {ok,<<"RIGHT\r\n">>} ->
      handle_right(PlayerSrv),
      loop(State);

    {ok,_UnknownData} ->
        Reply = <<"INVALID QUERY\r\n">>,
        Transport:send(Socket,Reply),
        loop(State);
		_ ->
      st_player_srv:stop(PlayerSrv),
			ok = Transport:close(Socket)
	end.

handle_ping()->
  <<"PONG\r\n">>.
handle_auth(LoginPass,PlayerSrv)->
  [Login,Pass1 | _] = binary:split(LoginPass,<<" ">>),
  Pass = binary:replace(Pass1,<<"\r\n">>,<<>>),
  case st_player_srv:auth(PlayerSrv,Login,Pass) of
    ok -> {ok,<<"SUCCES\r\n">>};
    error -> <<"INVALID LOGIN OR PASS\n PLEASE TRY ONE MORE\r\n">>
  end.

handle_reg(LoginPass,PlayerSrv)->
  [Login,Pass1 | _] = binary:split(LoginPass,<<" ">>),
  Pass = binary:replace(Pass1,<<"\r\n">>,<<>>),
  case st_player_srv:reg(PlayerSrv,Login,Pass) of
    ok -> {ok,<<"SUCCES\r\n">>};
    error -> <<"PLAYER WITH THIHS LOGIN ALREDY EXISTS\n PLEASE TRY ONE MORE\r\n">>
  end.

handle_game(PlayerSrv)->
  case st_player_srv:start_game(PlayerSrv) of
    ok -> {ok,<<"YOU GOT IN THE QUEUE TO SEARCH THE GAME\r\n">>};
    started -> ok;
    error -> <<"YOUR TOKEN HAS EXPIRED. PLEASE AUTH AGAIN\r\n">>
  end.

handle_up(PlayerSrv) ->
  st_player_srv:up(PlayerSrv).

handle_down(PlayerSrv) ->
  st_player_srv:down(PlayerSrv).

handle_left(PlayerSrv) ->
  st_player_srv:left(PlayerSrv).

handle_right(PlayerSrv) ->
  st_player_srv:right(PlayerSrv).
