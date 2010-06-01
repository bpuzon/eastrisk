%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% Manager example to originate call and receive it with AGI
%%% @end
%%%
%%% This example using originate/8 works if your dialplan has the following
%%% entries:
%%% [eastrisk]
%%% exten => _ZZ,1,Agi(agi://this_host:6666)
%%% exten => _ZZ,2,Hangup()
%%%
%%% ----------------------------------------------------------------------------
-module(ast_manager_zap).
-behaviour(gen_event).

-export([init/0, originate_7/0, originate_8/0]).
-export([
	handle_event/2,
	init/1,
	terminate/2,
	code_change/3,
	handle_call/2,
	handle_info/2
]).
-export([agi_answer/2]).

-include("agi.hrl").

%%% Exported API
init() ->
	agi_events:add_agi_handler(?MODULE, nil).


originate_7() ->
	Channel = "Zap/2",
	Application = "Agi",
	Data = "agi://192.168.1.13:6666",
	Timeout = 10000,
	CallerId = "test <42>", 
	Async = false,
	Variables = [],
	ast_manager:originate(Channel, Application, Data, Timeout, CallerId, Async,
                          Variables).

originate_8() ->
	Channel = "Zap/2",
	Context = "eastrisk",
	Extension = 42,
	Priority = 1,
	Timeout = 10000,
	CallerId = "test <42>", 
	Async = false,
	Variables = [],
	ast_manager:originate(Channel, Context, Extension, Priority, Timeout,
	                      CallerId, Async, Variables).

%%% gen_event callbacks
init(_) -> {ok, nil}.

handle_event({new_channel, Pid, Env}, State) ->
	% Pick up any call that has been connected to the AGI entry in the dialplan.
	spawn(?MODULE, agi_answer, [Pid, Env]),
	{ok, State};
handle_event({'Hangup', _Args}, State) ->
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

handle_call(_, State) -> {stop, not_supported, State}.
handle_info(_, State) -> {ok, State}.

terminate(_Reason, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{stop, not_supported, State}.

%%% Internal functions
agi_answer(ChPid, AgiEnv) ->
	agi:answer(ChPid),
	io:format("Call answered by AGI:\n~p\n", [AgiEnv]),
	%read_digits(ChPid),
	{ok, {1, Value}} = agi:get_full_variable(ChPid, "OSCAR"),
	io:format("~p\n", [Value]),
	agi:hangup(ChPid),
	agi_channel:close(ChPid),
	ok.

%read_digits(ChPid) ->
%	{ok, V} = agi:wait_for_digit(ChPid, 2000),
%	Value = list_to_integer([V]),
%	agi:say_number(ChPid, Value, ""),
%	if 
%		Value =:= 0 -> ok;
%		true -> read_digits(ChPid)
%	end.
