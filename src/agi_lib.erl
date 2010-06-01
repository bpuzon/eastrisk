%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.1, 2006-08-10
%%% @copyright 2006 Erlang Training and Consulting
%%% Library functions for AGI applications.
%%%
%%% These could come in handy ;)
%%% @doc
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi_lib).

-export([request_vars/1]).

%% -----------------------------------------------------------------------------
%% @spec request_vars(RequestStr) -> [{Name::atom(), Value::string()}]
%% @doc
%% Extracts variables sent with a FastAGI request from the request string.
%% <p>
%% The request string is passed to the event handler through the
%% <code>agi_env</code> record as the element <code>request</code>.
%% </p>
%% <p>
%% This can be used by doing the AGI call as:<br />
%% <code>AGI(agi://localhost:6666?var=value&#38;var_2=value_2)</code><br />
%% This function would then return
%% <code>[{var, "value"}, {var_2, "value_2"}]</code>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
request_vars(RequestStr) ->
	case  regexp:first_match(RequestStr, "\\?.*") of
		{match, Start, _} ->
			Vars = string:tokens(string:substr(RequestStr, Start + 1), "&"),
			lists:map(fun(Element) ->
				case string:tokens(Element, "=") of
					[Name, Value] -> ok;
					[Name]        -> Value = true
				end,
				{list_to_atom(Name), Value}
			end, Vars);
		_ ->
			[]
	end.
