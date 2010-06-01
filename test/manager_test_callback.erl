%%% (c) 2010 Erlang Solutions, Ltd 
-module(manager_test_callback).

-behaviour(manager).

-export([start_link/0]).

-export([init/1, handle_event/2, terminate/2, code_change/3]).

start_link() ->
	manager:start_link(?MODULE, "localhost", 5038, null).

init(null) ->
	{ok, null}.

handle_event(Info, State) ->
	io:format("~p\n", [Info]),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
