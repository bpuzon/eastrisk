%%% (c) 2010 Erlang Solutions, Ltd 
-module(partypants).

-export([start/2]).

start(ChannelPid, _ChannelEnv) ->
	agi:answer(ChannelPid),
	agi:stream_file(ChannelPid, "partypants", ""),
	read_response(ChannelPid),
	agi:say_number(ChannelPid, read_accept(ChannelPid), ""),
	agi:stream_file(ChannelPid, "have-accepted", ""),
	agi:say_number(ChannelPid, read_decline(ChannelPid), ""),
	agi:stream_file(ChannelPid, "have-declined", ""),
	agi:hangup(ChannelPid),
	agi_channel:close(ChannelPid).

read_response(ChannelPid) ->
	AcceptCount = read_accept(ChannelPid),
	DeclineCount = read_decline(ChannelPid),
	case agi:get_option(ChannelPid, "partyaccept", "12", 5000) of
		{ok, {$1, _EndPoint}} ->
			update_accept(ChannelPid, AcceptCount + 1),
			agi:stream_file(ChannelPid, "you", ""),
			agi:stream_file(ChannelPid, "have-accepted", "");
		{ok, {$2, _EndPoint}} ->
			update_decline(ChannelPid, DeclineCount + 1),
			agi:stream_file(ChannelPid, "you", ""),
			agi:stream_file(ChannelPid, "have-declined", "");
		{ok, {0, _EndPoint}} ->
			read_response(ChannelPid);
		{error, _Error} ->
			exit(normal)
	end.

update_accept(ChannelPid, Value) ->
	agi:database_put(ChannelPid, "partypants", "accept", integer_to_list(Value)).

update_decline(ChannelPid, Value) ->
	agi:database_put(ChannelPid, "partypants", "decline", integer_to_list(Value)).

read_accept(ChannelPid) ->
	case agi:database_get(ChannelPid, "partypants", "accept") of
		{ok, {1, Value}} -> list_to_integer(Value);
		{ok, 0}          -> 0;
		{error, _Error}  ->
			exit(normal)
	end.

read_decline(ChannelPid) ->
	case agi:database_get(ChannelPid, "partypants", "decline") of
		{ok, {1, Value}} -> list_to_integer(Value);
		{ok, 0}          -> 0;
		{error, _Error}  -> 
			exit(normal)
	end.
