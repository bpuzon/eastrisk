%%% ----------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.2, 2006-08-10
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% AGI Server.
%%% <p>
%%% This module provides an AGI Server which will listen to connections coming
%%% from a AGI -> Erlang part (see below). A child process is spawned upon an
%%% incoming connection and the initialisation arguments are read before control
%%% is handed to a callback module.
%%% </p>
%%% <p>
%%% A typical session looks like:
%%% FIXME:
%%% How does it look?
%%% </p>
%%% <p>
%%% AGI -> Erlang<br />
%%% To avoid taking a node up and down every time a channel is connected, a
%%% small C program is used to as a link between Asterisk and Erlang. The C
%%% program is launched when the AGI channel is connected and passes on all the
%%% data received from Asterisk to the AGI Server by a TCP connection.
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi_server).

%%% API
-export([start_link/2, stop/0]).

%%% Internal exports
-export([init/3]).

%%% OTP specific exports
-export([system_continue/3, system_terminate/4, system_code_change/4]).

%%% Records
-record(state, {socket}).

%%% ----------------------------------------------------------------------------
%%%                        API
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @spec start_link(Port::integer(), Debug) -> {ok, pid()}
%% @doc
%% Starts the AGI Server and links to the new process.
%% <p>
%% OBS! If the server is started successfully it is also registered as
%% agi_server.
%% </p>
%% <p>
%% The AGI Server will listen on the <em>Port</em> and will hand control to
%% <em>Callback</em> when a connection is made.
%% <em>Debug</em> are a list of debug options, see sys(3).
%% </p>
%% @end
%% -----------------------------------------------------------------------------
start_link(Port, Debug) ->
	proc_lib:start_link(?MODULE, init, [self(), Port, Debug]).

%% -----------------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc
%% Stops the AGI Server
%% @end
%% -----------------------------------------------------------------------------
stop() ->
	?MODULE ! stop.

%%% ----------------------------------------------------------------------------
%%%                         proc_lib functions
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% @spec init(Parent, Port, Debug) -> void()
%% @doc
%% Called when by proc_lib:start_link when a AGI Server is started.
%% @end
%% -----------------------------------------------------------------------------
init(Parent, Port, DebugOpts) ->
	{ok, Socket} = gen_tcp:listen(Port, [
		binary,
		{packet, line},
		{active, false}
	]),
	Debug = sys:debug_options(DebugOpts),
	register(?MODULE, self()),
	proc_lib:init_ack(Parent, {ok, self()}),
	loop(Parent, Debug, #state{socket = Socket}).

%% -----------------------------------------------------------------------------
%% @hidden
%% See sys:handle_system_msg
%% -----------------------------------------------------------------------------
system_continue(Parent, Debug, State) ->
	loop(Parent, Debug, State).

%% -----------------------------------------------------------------------------
%% @hidden
%% See sys:handle_system_msg
%% -----------------------------------------------------------------------------
system_terminate(Reason, _Parent, _Debug, _State) ->
	exit(Reason).

%% -----------------------------------------------------------------------------
%% @hidden
%% See sys:handle_system_msg
%% -----------------------------------------------------------------------------
system_code_change(State, _Module, _OldVsn, _State) ->
	{ok, State}.

%% -----------------------------------------------------------------------------
%%                         Internal functions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @hidden
%% Main loop of the AGI server
%% Waits for a connection and then starts a child process to handle the
%% connection.
%% -----------------------------------------------------------------------------
loop(Parent, Debug, State) ->
	receive
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
		stop ->
			gen_tcp:close(State#state.socket)
	after
		0 ->
			case gen_tcp:accept(State#state.socket, 100) of
				{ok, Socket} ->
					start_channel(Socket), 
					loop(Parent, Debug, State);
				{error, timeout} ->
					loop(Parent, Debug, State);
				{error, Reason} ->
					gen_tcp:close(State#state.socket),
					exit(Reason)
			end
	end.

start_channel(Socket) ->
	agi_channel_sup:start_channel(Socket).
