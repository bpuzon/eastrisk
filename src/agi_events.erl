%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.3, 2007-05-17
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% Event manager for AGI channels.
%%% <p>
%%% Event handlers can subscribe to incoming calls through an AGI channel by
%%% adding themselves to this event manager. When a call is connected through a
%%% AGI channel will be spawned and event handlers will be notified.
%%% </p>
%%% <p>
%%% Event handlers are added to this manager using {@link add_agi_handler} and
%%% removed using {@link delete_agi_handler}.
%%% A handler must be able to handle events of the type: <br />
%%% <code>{new_channel, ChannelPid, ChanEnv}</code>,<br />
%%% where <em>ChanEnv</em> is a record of type <code>agi_env()</code>.
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi_events).

%%% API Exports
-export([start_link/0,
         call/2,
		 add_agi_handler/2,
		 delete_agi_handler/1]).

%%% Intermodule exports
-export([notify_new_channel/2]).

%% -----------------------------------------------------------------------------
%% @private
%% @spec start_link() -> {ok, Pid}
%% @doc
%% Starts the event manager
%% @end
%% -----------------------------------------------------------------------------
start_link() ->
	gen_event:start_link({local, ?MODULE}).

%% -----------------------------------------------------------------------------
%% @spec call(Handler, Request) -> term()
%% @doc
%% Performes a gen_event:call/3 to this event manager.
%% <p>
%% The <em>Handler</em> specified must be able to handle the <em>Request</em>.
%% This function will never be used by the eastrisk application, i.e. no calls
%% to event handlers will be issued from the eastrisk application, and is 
%% here to enable custom calls to event handlers.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
call(Handler, Request) ->
	gen_event:call(?MODULE, Handler, Request).

%% -----------------------------------------------------------------------------
%% @spec add_agi_handler(Module, Args) -> ok | {'EXIT',Reason} | term()
%% @doc
%% Adds an event handler to the event manager.
%% <p>
%% For more information see gen_event(3).
%% </p>
%% @end
%% -----------------------------------------------------------------------------
add_agi_handler(Module, Args) ->
	gen_event:add_handler(?MODULE, Module, Args).

%% -----------------------------------------------------------------------------
%% @spec delete_agi_handler(Module) -> term() | {error,module_not_found} |
%%                                     {'EXIT',Reason}
%% @doc
%% Deletes an event handler from the event manager.
%% <p>
%% see gen_event(3).
%% </p>
%% @end
%% -----------------------------------------------------------------------------
delete_agi_handler(Module) ->
	gen_event:delete_handler(?MODULE, Module, []).

%% -----------------------------------------------------------------------------
%%                  Intermodule exports
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @private
%% @spec notify_new_channel(ChannelPid::pid(), ChannelEnv::list()) -> ok
%% @doc
%% Called no notify listeners that a new channel has been opened.
%% <em>ChannelPid</em> is the pid of the channel process.
%% <em>ChannelEnv</em> is a list of channel environment variables.
%% @end
%% -----------------------------------------------------------------------------
notify_new_channel(ChannelPid, ChannelEnv) ->
	gen_event:notify(?MODULE, {new_channel, ChannelPid, ChannelEnv}).

%%% ----------------------------------------------------------------------------
%%%                Type definitions
%%% ----------------------------------------------------------------------------
%%% @type agi_env() = record().
%%% <pre>
%%% -record(agi_env,
%%%        {
%%%         network,      = bool()
%%%         request,      = string()
%%%         channel,      = string()
%%%         language,     = string()
%%%         type,         = string()
%%%         uniqueid,     = string()
%%%         callerid,     = string()
%%%         calleridname, = string()
%%%         callingpres,  = int()
%%%         callingani2,  = int()
%%%         callington,   = int()
%%%         callingtns,   = int()
%%%         dnid,         = string()
%%%         rdnis,        = string()
%%%         context,      = string()
%%%         extension,    = string()
%%%         priority,     = string()
%%%         enhanced,     = string()
%%%         accountcode   = string()
%%%        }).
%%% </pre>
%%% More information is available in the Asterisk documentation.
%%% @end
%%% ----------------------------------------------------------------------------
