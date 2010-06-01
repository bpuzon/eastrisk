%%% ----------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.1, 2006-07-05
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% Supervisor for AGI Channels.
%%% <p>
%%% Used to start a new process when a channel is connected by Asterisk.
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi_channel_sup).
-behaviour(supervisor).

%%% API exports
-export([start_link/0, start_channel/1]).

%%% supervisor callbacks
-export([init/1]).

%% -----------------------------------------------------------------------------
%%                                API
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% @spec start_link() -> {ok, Pid}
%% @doc
%% Starts the supervisor, registered as agi_channel_sup.
%% @end
%% -----------------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, null).

%% -----------------------------------------------------------------------------
%%                      supervisor callbacks
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
init(null) ->
	ChannelSpec = {null, {agi_channel, start_link, []},
	               temporary, 1000, worker, [agi_channel]},
	{ok, {{simple_one_for_one, 0, 1}, [ChannelSpec]}}.

%% -----------------------------------------------------------------------------
%%                       Intermodule functions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% @spec start_channel(Socket::socket()) -> {ok, Pid}
%% @doc 
%% Starts a new channel which will communicate through <em>Socket</em>
%% @end
%% -----------------------------------------------------------------------------
start_channel(Socket) ->
	case supervisor:start_child(?MODULE, [Socket]) of
		{error, Error} -> exit(Error);
		Return         -> Return
	end.

%%% @type socket(). See gen_tcp(3).
