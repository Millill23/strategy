-module(strategy_worker).
-behavior(gen_server).

-export([start_link/2,hello/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        user_id,
        user_name
}).


%%% module API

start_link(UserID,UserName) ->
    gen_server:start_link(?MODULE, {UserID,UserName}, []).

hello()->
    42.

init({UserID,UserName}) ->
    State = #state{user_id = UserID,user_name = UserName},
    lager:info("State is ~p ~p",[State,self()]),
    lager:warning("warning"),
    lager:error("error"),
    {ok,State}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
{ok, State}.
