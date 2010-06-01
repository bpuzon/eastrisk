%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%% (c) 2010 Erlang Solutions, Ltd 
%%% @doc
%%% Test module for the Erlang AGI interface.
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi_event_handler).
-behaviour(gen_event).

%%% API Exports
-export([start/0]).

-export([init/1,
         handle_event/2,
		 handle_call/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-include("agi.hrl").

start() ->
	agi_events:add_agi_handler(?MODULE, null).

init(null) ->
	{ok, null}.

handle_event({new_channel, ChannelPid, ChanEnv}, State) ->
	io:format("Channel opened: ~p\n", [ChanEnv]),
	agi_channel:close(ChannelPid),
	%case ChanEnv#agi_env.extension of
		%"49" ->
		%	spawn(beer, start, [ChannelPid, ChanEnv]);
		%_Else ->
		%	spawn(partypants, start, [ChannelPid, ChanEnv])
	%end,
	{ok, State}.

handle_call(_Request, _State) ->
	{remove_handler, {error, uknown_call}}.

handle_info(_Info, _State) ->
	remove_handler.

terminate(_Reason, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
