%%% ----------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.3, 2006-08-08
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% @end
%%% ----------------------------------------------------------------------------
-module(ast_manager_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% -----------------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid}
%% @doc
%% Starts the supervisor.
%% @end
%% -----------------------------------------------------------------------------
start_link() ->
	supervisor:start_link(?MODULE, null).

init(null) ->
	{ok, Port}   = application:get_env(mgr_port),
	{ok, Host}   = application:get_env(mgr_host),
	{ok, Name}   = application:get_env(mgr_name),
	{ok, Passwd} = application:get_env(mgr_secret),

	MgrEventMgrSpec = {ast_manager_events, {ast_manager_events, start_link, []},
				   permanent, 1000, worker, [ast_manager_events]},
	MgrServerSpec   = {ast_manager_server, {ast_manager_server, start_link,
	                                    [Host, Port, Name, Passwd]},
	               permanent, 1000, worker, [ast_manager_server]},
	{ok, {{one_for_one, 4, 5}, [MgrEventMgrSpec, MgrServerSpec]}}.
