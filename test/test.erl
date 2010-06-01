%%% (c) 2010 Erlang Solutions, Ltd 
-module(test).
-export([start/0, start_aux/0]).
-export([handle_event/2, init/1, terminate/2, code_change/3, handle_call/2,
handle_info/2]).
-export([make_call/1, poll/1, agi/2]).
-behaviour(gen_event).

start() ->
	spawn(?MODULE, start_aux, []).

start_aux() ->
	process_flag(trap_exit, true),
	manager_event_mgr:add_manager_handler(?MODULE, null),
	agi_event_mgr:add_agi_handler(?MODULE, null),
	PollList = spawn_poll(100, []),
	CallList = spawn_calling_processes(1, []),
	loop(PollList, CallList).

loop(PollList, CallList) ->
	receive
		{'EXIT', From, Reason} ->
			io:format("Exited: ~p\n~p\n", [From, Reason]),
			loop(PollList, CallList);
		stop ->
			stop(PollList),
			stop(CallList)
	end.

stop([H|T]) ->
	exit(H, kill),
	stop(T);
stop([]) ->
	ok.

spawn_poll(0, Acc) ->
	Acc;
spawn_poll(Value, Acc) ->
	spawn_poll(Value - 1, [spawn_link(?MODULE, poll, [Value])|Acc]).

spawn_calling_processes(0, Acc) ->
	Acc;
spawn_calling_processes(Count, Acc) ->
	spawn_calling_processes(Count - 1, [spawn_link(?MODULE, make_call, [0])|Acc]).

poll(Value) ->
	manager:status(),
	io:format("~p: Poll\n", [Value]),
	manager:sip_peers(),
	timer:sleep(100),
	poll(Value).

make_call(Value) ->
	manager:originate("SIP/wip5000", "internal", 40 + Value, 1, 10000,
	                  "anon", false, []),
	make_call(next(Value)).

next(9)     -> 0;
next(Value) -> Value + 1.

init(null) ->
	{ok, null}.

handle_event({new_channel, Pid, Env}, State) ->
	spawn(?MODULE, agi, [Pid, Env]),
	{ok, State};
handle_event({'Hangup', _Args}, State) ->
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

agi(Pid, Env) ->
	io:format("Connected call on ext: ~p\n",
	          [agi_channel:env_var(Env, agi_extension)]),
	agi:answer(Pid),
	agi:stream_file(Pid, "carried-away-by-monkeys", ""),
	agi:hangup(Pid),
	agi_channel:close(Pid).

handle_call(_Request, _State) ->
	{remove_handler, {error, uknown_call}}.

handle_info(_Info, _State) ->
	remove_handler.

terminate(_Reason, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
