%%% ----------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.2, 2006-08-02
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% @end
%%% ----------------------------------------------------------------------------
-module(eastrisk_sup).

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
	MgrSpec = {mgr, {ast_manager_sup, start_link, []},
				   permanent, infinity, supervisor, [ast_manager_sup]},
	AgiSpec = {agi, {agi_sup, start_link, []},
				   permanent, infinity, supervisor, [agi_sup]},

	{ok, Mgr} = application:get_env(eastrisk, mgr_server),
	{ok, AGI} = application:get_env(eastrisk, agi_server),

	ChildList = if
		Mgr and AGI -> [MgrSpec, AgiSpec];
		AGI         -> [AgiSpec];
		Mgr         -> [MgrSpec];
		true        -> []
	end,
	{ok, {{one_for_one, 0, 1}, ChildList}}.
