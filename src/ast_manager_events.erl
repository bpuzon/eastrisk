%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.1, 2006-07-11
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% Event manager for the Asterisk Manager Interface.
%%% <p>
%%% If the manager server of the eastrisk application is running, it is
%%% possible to use this event manager to receive Manager events.
%%% </p>
%%% <p>
%%% The registered module must be a gen_event where <code>handle_event/2</code> 
%%% will be called as <code><b>Callback:</b>handle_event(Event, State)</code>.
%%% State is an arbitrary term returned by <code><b>Callback:</b>init/1</code>, 
%%% which is called once when the callback is registered.
%%% The return value of the <code>handle_event/2</code> must conform to the 
%%% specification of gen_event(3).
%%% </p>
%%% <p>
%%% An event looks like:<br /><br />
%%% <code>{Type, Record}</code><br />
%%% <ul>
%%%     <li><code>Type = atom()</code></li>
%%%     <li><code>Record = record()</code></li>
%%% </ul>
%%% </p>
%%% <h3>Events</h3>
%%% <h4>'Alarm'</h4>
%%% -record(alarm, {
%%%     privilege,        % [atom()]
%%%     alarm,            % string()
%%%     channel           % integer()
%%% }).
%%% <h4>'AlarmClear'</h4>
%%% -record(alarm, {
%%% }).
%%% <h4>'LogChannel'</h4>
%%% <pre>
%%% -record(log,
%%% {
%%%     privilege,        % [atom()]
%%%     channel,          % string()
%%%     enabled,          % bool()
%%%     reason            % string()
%%% }).
%%% </pre>
%%% <h4>'DNDState'</h4>
%%% <pre>
%%% -record(dnd_state,
%%% {
%%%     privilege,        % [atom()]
%%%     channel,          % string()
%%%     status            % enabled | disabled
%%% }).
%%% </pre>
%%% <h4>'Join'</h4>
%%% <pre>
%%% -record(queue_entry,
%%% {
%%%     privilege,      % [atom()]
%%%     queue,          % string()
%%%     position,       % integer()
%%%     channel,        % string()
%%%     caller_id,      % string()
%%%     caller_id_name, % string()
%%%     wait,           % integer()
%%%     count           % integer()
%%% }).
%%% </pre>
%%% <h4>'Leave'</h4>
%%% <pre>
%%% -record(queue_entry, {
%%%     privilege,      % [atom()]
%%%     queue,          % string()
%%%     position,       % integer()
%%%     channel,        % string()
%%%     caller_id,      % string()
%%%     caller_id_name, % string()
%%%     wait,           % integer()
%%%     count           % integer()
%%% }).
%%% </pre>
%%% <h4>'Registry'</h4>
%%% <pre>
%%% -record(registry,
%%% {
%%% }).
%%% </pre>
%%% <h4>'Hold'</h4>
%%% <pre>
%%% -record(hold,
%%% {
%%% }).
%%% </pre>
%%% <h4>'Unhold'</h4>
%%% <pre>
%%% -record(hold,
%%% {
%%% }).
%%% </pre>
%%% <h4>'Hangup'</h4>
%%% <pre>
%%% -record(hangup,
%%% {
%%%     privilege,         % [atom()]
%%%     channel,           % string()
%%%     cause,             % integer()
%%%     cause_txt,         % string()
%%%     unique_id          % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Link'</h4>
%%% <pre>
%%% -record(link,
%%% {
%%%     privilege,         % [atom()]
%%%     channel_1,         % string()
%%%     channel_2,         % string()
%%%     unique_id_1,       % string()
%%%     unique_id_2,       % string()
%%%     caller_id_1,       % string()
%%%     caller_id_2        % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Unlink'</h4>
%%% <pre>
%%% -record(link,
%%% {
%%%     privilege,         % [atom()]
%%%     channel_1,         % string()
%%%     channel_2,         % string()
%%%     unique_id_1,       % string()
%%%     unique_id_2,       % string()
%%%     caller_id_1,       % string()
%%%     caller_id_2        % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Newexten'</h4>
%%% <pre>
%%% -record(exten,
%%% {
%%%     privilege,         % [atom()]
%%%     channel,           % string()
%%%     context,           % string()
%%%     exten,             % string()
%%%     priority,          % string()
%%%     application,       % string()
%%%     app_data,          % string()
%%%     unique_id          % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Newstate'</h4>
%%% <pre>
%%% -record(ast_state,
%%% {
%%%     privilege,         % [atom()]
%%%     channel,           % string()
%%%     state,             % atom()
%%%     caller_id,         % string()
%%%     caller_id_name,    % string()
%%%     unique_id          % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Newchannel'</h4>
%%% <pre>
%%% -record(channel,
%%% {
%%%     privilege,         % [atom()]
%%%     channel,           % string()
%%%     state,             % atom()
%%%     caller_id,         % string()
%%%     caller_id_name,    % string()
%%%     unique_id          % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Newcallerid'</h4>
%%% <pre>
%%% -record(caller_id,
%%% {
%%%    privilege,         % [atom()]
%%%    channel,           % string()
%%%    caller_id,         % string()
%%%    caller_id_name,    % string()
%%%    unique_id,         % string()
%%%    cid_calling_pres   % integer()
%%% }).
%%% </pre>
%%%
%%% <h4>'QueueMemberPaused'</h4>
%%% <pre>
%%% -record(queue_member,
%%% {
%%%     privilege,   % [atom()]
%%%     queue,       % string()
%%%     location,    % string()
%%%     membership,  % string()
%%%     penalty,     % integer()
%%%     calls_taken, % integer()
%%%     last_call,   % integer()
%%%     status,      % integer()
%%%     paused       % bool()
%%% }).
%%% </pre>
%%%
%%% <h4>'QueueMemberAdded'</h4>
%%% <pre>
%%% -record(queue_member,
%%% {
%%%     privilege,   % [atom()]
%%%     queue,       % string()
%%%     location,    % string()
%%%     membership,  % string()
%%%     penalty,     % integer()
%%%     calls_taken, % integer()
%%%     last_call,   % integer()
%%%     status,      % integer()
%%%     paused       % bool()
%%% }).
%%% </pre>
%%%
%%% <h4>'QueueMemberRemoved'</h4>
%%% <pre>
%%% -record(queue_member,
%%% {
%%%     privilege,   % [atom()]
%%%     queue,       % string()
%%%     location,    % string()
%%%     membership,  % string()
%%%     penalty,     % integer()
%%%     calls_taken, % integer()
%%%     last_call,   % integer()
%%%     status,      % integer()
%%%     paused       % bool()
%%% }).
%%% </pre>
%%%
%%% <h4>'ParkedCallTimeOut'</h4>
%%% <pre>
%%% -record(park_call,
%%% {
%%%     privilege,     % [atom()]
%%%     exten,         % string()
%%%     channel,       % string()
%%%     from,          % string()
%%%     timeout,       % integer()
%%%     caller_id,     % string()
%%%     caller_id_name % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'ParkedCallGiveUp'</h4>
%%% <pre>
%%% -record(park_call,
%%% {
%%%     privilege,     % [atom()]
%%%     exten,         % string()
%%%     channel,       % string()
%%%     from,          % string()
%%%     timeout,       % integer()
%%%     caller_id,     % string()
%%%     caller_id_name % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Agentcallbacklogoff'</h4>
%%% <pre>
%%% -record(agent_event,
%%% {
%%%     privilege,         % [atom()]
%%%     agent_id,          % integer()
%%%     login_chan,        % string()
%%%     login_time         % integer()
%%% }).
%%% </pre>
%%%
%%% <h4>'Agentcallbacklogin'</h4>
%%% <pre>
%%% -record(agent_event,
%%% {
%%%     privilege,         % [atom()]
%%%     agent_id,          % integer()
%%%     login_chan,        % string()
%%%     login_time         % integer()
%%% }).
%%% </pre>
%%%
%%% <h4>'Dial'</h4>
%%% <pre>
%%% -record(call,
%%% {
%%%     privilege,         % [atom()]
%%%     source,            % string()
%%%     destination,       % string()
%%%     caller_id,         % string()
%%%     caller_id_name,    % string()
%%%     src_unique_id,     % string()
%%%     dest_unique_id     % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'Reload'</h4>
%%% <pre>
%%% -record(reload,
%%% {
%%%     privilege,         % [atom()]
%%%     message            % string()
%%% }).
%%% </pre>
%%%
%%% <h4>'QueueMemberStatus'</h4>
%%% <pre>
%%% -record(queue_member,
%%% {
%%%     privilege,         % [atom()]
%%%     queue,             % string()
%%%     location,          % string()
%%%     membership,        % string()
%%%     penalty,           % integer()
%%%     calls_taken,       % integer()
%%%     last_call,         % integer()
%%%     status,            % integer()
%%%     paused             % bool()
%%% }).
%%% </pre>
%%%
%%% <p>
%%% See gen_event(3).
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(ast_manager_events).

%%% API exports
-export([start_link/0,
		 add_manager_handler/2,
		 delete_manager_handler/1]).

%%% Intermodule exports
-export([notify_event/1]).

%% -----------------------------------------------------------------------------
%% @private
%% @spec start_link() -> {ok, Pid}
%% @doc
%% Starts the event manager.
%% @end
%% -----------------------------------------------------------------------------
start_link() ->
	gen_event:start_link({local, ?MODULE}).

%% -----------------------------------------------------------------------------
%% @spec add_manager_handler(Module::atom(), Args::term()) ->
%%                                              ok | {'EXIT', Reason} | term()
%% @doc
%% Adds an event handler to the event manager.
%% See gen_event(3).
%% @end
%% -----------------------------------------------------------------------------
add_manager_handler(Module, Args) ->
	gen_event:add_handler(?MODULE, Module, Args).

%% -----------------------------------------------------------------------------
%% @spec delete_manager_handler(Module::atom()) -> term()
%% @doc
%% Removes an event handler for the event manager.
%% The returned term is whatever is returned by <em>Module</em>:terminate/2.
%% @end
%% -----------------------------------------------------------------------------
delete_manager_handler(Module) ->
	gen_event:delete_handler(?MODULE, Module, []).

%% -----------------------------------------------------------------------------
%% @private
%% @spec notify_event(Event::tuple()) -> ok
%% @doc
%% Notifies the registered event handlers.
%% @end
%% -----------------------------------------------------------------------------
notify_event(Event) ->
	gen_event:notify(?MODULE, Event).
