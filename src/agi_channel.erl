%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.3, 2006-08-10
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% This module represents a channel process.
%%% <p>
%%% When the AGI Server gets a connection, it spawns an AGI Channel using this
%%% module. When in initialisation is done this module will use the {@link
%%% agi_events} to notify event handlers about the new channel.
%%% When the program has finished, {@link agi_channel:close/2} must be called. 
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi_channel).
-behaviour(gen_server).
-vsn('0.0.3').

%%% API exports
-export([start_link/1, send/2, close/1]).

%%% Intermodule exports
-export([init/1,
         handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 code_change/3,
		 terminate/2]).

-include("agi.hrl").

-record(state, {socket}).

-define(TIMEOUT, 20000).

%%% ----------------------------------------------------------------------------
%%%                  API
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% @spec start_link(Socket) -> {ok, Pid::pid()}
%% @doc
%% Starts the AGI Channel process.
%% @end
%% -----------------------------------------------------------------------------
start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

%% -----------------------------------------------------------------------------
%% @spec close(ChannelPid) -> ok
%% @doc
%% Closes the channel.
%% @end
%% -----------------------------------------------------------------------------
close(ChannelPid) ->
	gen_server:call(ChannelPid, close).

%%% ----------------------------------------------------------------------------
%%%                  gen_server callbacks
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% @spec init(Socket::socket()) -> void()
%% @doc
%% Reads initialisation arguments from Asterisk before handing control to
%% <em>Callback</em>.
%% @end
%% -----------------------------------------------------------------------------
init(Socket) ->
	State = #state{socket = Socket},
	case read_args(Socket, #agi_env{}) of
		{error, Reason} ->
			{stop, Reason};
		{ok, ChanEnv} ->
			agi_events:notify_new_channel(self(), ChanEnv),
			{ok, State, ?TIMEOUT}
	end.

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
handle_call(close, _From, State) ->
	{stop, normal, ok, State};
handle_call({send, Command}, _From, State) ->
	gen_tcp:send(State#state.socket, Command),
	Reply = gen_tcp:recv(State#state.socket, 0),
	{reply, Reply, State, ?TIMEOUT};
handle_call(Request, From, State) ->
	{stop, {unhandled_call, {From, Request}}, State}.

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
handle_cast(Request, State) ->
	{stop, {unhandled_cast, Request}, State}.

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
handle_info(timeout, State) ->
	{stop, timeout, State};
	%agi:verbose(State#state.socket, Message, 3), % FIXME deadlock
handle_info(Info, State) ->
	{stop, {unhandel_info, Info}, State}.

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
terminate(_Reason, State) ->
	gen_tcp:close(State#state.socket).

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% -----------------------------------------------------------------------------
%%                 Intermodule functions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% -----------------------------------------------------------------------------
send(ChannelPid, Command) ->
	gen_server:call(ChannelPid, {send, Command}, infinity).

%%% ----------------------------------------------------------------------------
%%%                  Internal functions
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
read_args(Socket, Record) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			case Bin of
				<<"\n">> -> % end of initialisation
					{ok, Record};
				_Else ->
					Line = strip_last_byte(Bin, size(Bin)),
					read_args(Socket, agi:parse_init_arg(Line, Record))
			end;
		{error, closed} ->
			gen_tcp:close(Socket),
			{error, socket_closed}
	end.

strip_last_byte(Binary, TSize) ->
	Size = TSize -1,
	<<Value:Size/binary, _:1/binary>> = Binary,
	Value.
