%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%% @private
%%% @version 0.1, 2006-08-10
%%% @copyright 2006 Erlang Training and Consulting
%%% @doc
%%% @end
%%% ----------------------------------------------------------------------------
-module(eastrisk_types).
-export([
	to_bool/1
	]).
%% -----------------------------------------------------------------------------
%% @spec to_bool(Binary::binary()) -> bool()
%% @doc
%% Converts an asterisk boolean value to an Erlang boolean.
%% @end
%% -----------------------------------------------------------------------------
to_bool(<<"yes">>)     -> true;
to_bool(<<"Enabled">>) -> true;
to_bool(<<"True">>)    -> true;
to_bool(<<"Y">>)       -> true;
to_bool(<<"1">>)       -> true;
to_bool(_)             -> false.
