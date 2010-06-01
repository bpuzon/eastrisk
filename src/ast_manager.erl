%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.3, 2006-08-08
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% This module provides a behaviour and library functions for for the Asterisk
%%% Manager interface.
%%% <p>
%%% The implementing callback must export the function
%%% <code>handle_event/2</code>, which
%%% will be called when a Asterisk Manager Event has been received.
%%% </p>
%%% <p>
%%% If the manager event manager ({@link ast_manager_events}) is used, this
%%% behaviour is already implemented
%%% by the Eastrisk application, and events will be distributed by the event
%%% manager. To perform actions however, the library functions in this module
%%% will be used.
%%% </p>
%%% <p>
%%% To use the manager, and understand the return values, please include
%%% ast_mgr.hrl
%%% </p>
%%% <p>
%%% IAXPeers implemented 2009-11-20 by bartlomiej.puzon@erlang-consulting.com
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(ast_manager).
-behaviour(gen_server).
-vsn('0.1').

%%% API functions
-export([start_link/4, stop/0, behaviour_info/1]).

%%% Asterisk Manager Interface API
-export([absolute_timeout/2,
         agent_callback_login/5,
         agent_logoff/1,
         agents/0,
         change_monitor/2,
         command/1,
         db_get/2,
         db_put/3,
         extension_state/2,
         events/1,
         get_var/1,
         get_var/2,
         hangup/1,
         iax_netstats/0,
	 iax_peers/0,
         list_commands/0,
         login/2,
         logoff/0,
         mailbox_count/1,
         mailbox_status/1,
         monitor/2,
         monitor/3,
         monitor/4,
         originate/7,
         originate/8,
         parked_calls/0,
         ping/0,
         play_dtmf/2,
         queue_add/4,
         queue_pause/3,
         queue_remove/2,
         queues/0,
         queue_status/0,
         redirect/4,
         redirect/5,
         set_cdr_user_field/3,
         set_var/2,
         set_var/3,
         sip_peers/0,
         sip_show_peer/1,
         status/0,
         status/1,
         stop_monitor/1,
         zap_dial_off_hook/2,
         zap_dnd_off/1,
         zap_dnd_on/1,
         zap_hangup/1,
         zap_show_channels/0,
         zap_transfer/1]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ast_mgr.hrl").

-record(state, {socket, callback, callback_state, pkg_acc, pkg_tbl, reply_tbl}).

-define(NAME, ast_manager).

%% -----------------------------------------------------------------------------
%% @spec start_link(Callback::atom(), Host::address(), Port::integer(),
%%                  Args::term) -> {ok, Pid::pid()}
%% @doc
%% Starts the manager and links to the calling process.
%% <p>
%% This function should be called from a supervisor.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
start_link(Callback, Host, Port, Args) ->
	gen_server:start_link({local, ?NAME}, ?MODULE,
	                      {Callback, Host, Port, Args}, []).

%% -----------------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc
%% Stops the manager.
%% <p>
%% This will cause the manager to call the Callback:terminate/2 function with
%% reason normal.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
stop() ->
	gen_server:call(?NAME, '__stop').

%% -----------------------------------------------------------------------------
%% @hidden
%% -----------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{handle_event, 2}, {init, 1}, {terminate, 2}, {code_change, 3}];
behaviour_info(_Other) ->
    undefined.

%%% ----------------------------------------------------------------------------
%%%                 Asterisk Manager Interface functions
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @spec absolute_timeout(Channel::string(), Timeout::integer()) -> 
%%                                                        {Status, Message}
%%                                    Status  = ok | error
%%                                    Message = string()
%% @doc
%% Hangup a channel after a certain time.
%% <p>
%% Hangs up the channel <em>Channel</em> after <em>Timeout</em> seconds.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
absolute_timeout(Channel, Timeout) ->
	Cmd = action("AbsoluteTimeout", [{"Channel", Channel},
	                                 {"Timeout", Timeout}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec agent_callback_login(Agent::string(), Exten::string(),
%%                            Context::string(), AckCall::bool(),
%%                            WrapupTime::integer()) -> mgr_response()
%% @doc
%% Sets an agent as logged in with callback.
%% <p>
%% Sets the <em>Agent</em> as logged in with the <em>Exten</em> in
%% <em>Context</em> as callback. If <em>AckCall</em> is <code>true</code> the
%% agent will have to acknowledge the call with the <em>#</em> key before it is
%% answered. <em>WrapupTime</em> is the amount of milliseconds after
%% disconnecting the caller(?) can receive a new call.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
agent_callback_login(Agent, Exten, Context, AckCall, WrapupTime) ->
	Cmd = action("AgentCallBackLogin", [{"Agent", Agent},
	                                    {"Exten", Exten},
	                                    {"Context", Context},
	                                    {"AckCall", AckCall},
	                                    {"WrapupTime", WrapupTime}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec agent_logoff(Agent) -> {Stats, Message}
%%                                    Status  = ok | error
%%                                    Message = string()
%% @doc
%% Sets an agent as no longer logged in.
%% @end
%% -----------------------------------------------------------------------------
agent_logoff(Agent) ->
	Cmd = action("AgentLogoff", [{"Agent", Agent}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec agents() -> {ok, Agents}
%%                   Agents = [Agent]
%%                   Agent = record()
%% @doc
%% Will list information about all available agents.
%% <p>
%% <pre>
%% -record(agent,
%% {
%%     id,        % int()
%%     name,      % string()
%%     status,    % atom()
%%     channel,   % string()
%%     time,      % int()
%%     talking_to % string()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
agents() ->
	Cmd = action("Agents", []),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec change_monitor(Channel::string(), File::string()) -> mgr_response()
%% @doc
%% Changes the file used to monitor a channel.
%% <p>
%% Will make the audio to be saved to another file then then original one
%% specified by {@link monitor/2}, {@link monitor/3} or {@link monitor/4}
%% function.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
change_monitor(Channel, File) ->
	Cmd = action("ChangeMonitor", [{"Channel", Channel}, {"File", File}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec command(Command::string()) -> mgr_response()
%% @doc
%% Execute an Asterisk CLI command.
%% <p>
%% NOTE: Does not return anything useful in Asterisk 1.2.9.1 or earlier.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
command(Command) ->
	Cmd = action("Command", [{"Command", Command}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec db_get(Family, Key) -> {ok, Value} | {error, Message}
%%              Value   = string()
%%              Message = string()
%% @doc
%% Retrieves a value for the Asterisk DB.
%% @end
%% -----------------------------------------------------------------------------
db_get(Family, Key) ->
	Cmd = action("DBGet", [{"Family", Family}, {"Key", Key}]),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec db_put(Family::string(), Key::string(), Value::string()) ->
%%                                                    {Status, Message}
%%                   Status  = ok | error
%%                   Message = string()
%% @doc
%% Inserts a value in the Asterisk database.
%% @end
%% -----------------------------------------------------------------------------
db_put(Family, Key, Value) ->
	Cmd = action("DBPut", [{"Family", Family}, {"Key", Key}, {"Val", Value}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec extension_state(Exten::string(), Context::string()) -> {ok, State}
%%                                                   State = record()
%% @doc
%% Report the extension state for given extension.
%% <p>
%% <pre>
%% -record(exten_state,
%% {
%%     status,  % integer()
%%     hint,    % string()
%%     context, % string()
%%     exten    % string()
%% }).  
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
extension_state(Exten, Context) ->   
	Cmd = action("ExtensionState", [{"Exten", Exten}, {"Context", Context}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec events(EventMask) -> ok
%%        EventMask = true | false | List
%%        List = [Flag]
%%        Flag = call | system | log
%% @doc
%% Enable/Disable sending of events to this manager.
%% <p>
%% Please note that that this would affect all event handlers connected to the
%% manager event manager.
%% </p>
%% <p>
%% <em>EventMask</em> must not include more than three flags.
%% </p>
%% <p>
%% Returns <code>ok</code> if <em>EventMask</em> is <code>true</code>, otherwise
%% a normal mgr_response().
%% </p>
%% @end
%% -----------------------------------------------------------------------------
events(false) ->
	Cmd = action("Events", [{"EventMask", off}]),
	{Status, _} = send_cmd(Cmd),
	Status;
events(true) ->
	Cmd = action("Events", [{"EventMask", on}]),
	send_async_cmd(Cmd);
events(List) when is_list(List) ->
	EventMask = event_flags(List),
	Cmd = action("Events", [{"EventMask", EventMask}]),
	{Status, _} = send_cmd(Cmd),
	Status.

%% -----------------------------------------------------------------------------
%% @spec get_var(Variable::string()) -> {ok, Value} | {error, Message}
%%                         Value   = string()
%%                         Message = string()
%% @doc
%% Get the value of a global variable.
%% @end
%% -----------------------------------------------------------------------------
get_var(Variable) ->
	Cmd = action("Getvar", [{"Variable", Variable}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec get_var(Channel::string(), Variable::string()) -> 
%%                                            {ok, Value} | {error, Message}
%%                                            Value   = string()
%%                                            Message = string()
%% @doc
%% Get the value of a channel variable.
%% @end
%% -----------------------------------------------------------------------------
get_var(Channel, Variable) ->
	Cmd = action("Getvar", [{"Channel", Channel}, {"Variable", Variable}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec hangup(Channel::string()) -> {Status, Message}
%%                          Status  = ok | error
%%                          Message = string()
%% @doc
%% Hangs up a channel.
%% @end
%% -----------------------------------------------------------------------------
hangup(Channel) ->
	Cmd = action("Hangup", [{"Channel", Channel}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec iax_netstats() -> ok
%% @doc
%% Show IAX Netstats.
%% <p>
%% NOTE: This does not seem to return anything useful in Asterisk 1.2.9.1 or
%% earlier.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
iax_netstats() ->
	Cmd = action("IAXnetstats", []),
	send_async_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec list_commands() -> mgr_response()
%% @doc
%% Returns the action name and synopsis for every action that is available to
%% the user.
%% NOTE: The return is just a list of strings, not very good for a machine.
%% @end
%% -----------------------------------------------------------------------------
list_commands() ->
	Cmd = action("ListCommands", []),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec login(Name::string(), Passwd::string()) -> mgr_response()
%% @doc
%% Login using MD5 challenge.
%% @end
%% -----------------------------------------------------------------------------
login(Name, Passwd) ->
	Challenge = action("Challenge", [{"AuthType", "MD5"}]),
	{ok, Challange} = send_cmd(Challenge),
	Key = hex(erlang:md5(Challange ++ Passwd)),
	Login = action("Login", [{"AuthType", "MD5"},
							 {"UserName", Name},
							 {"Key", Key}]),
	case send_cmd(Login) of
		{ok, _} -> ok;
		Error -> Error
	end.


%% -----------------------------------------------------------------------------
%% @spec logoff() -> ok | {error, Message}
%%                         Message = string()
%% @doc
%% Logs of the manager.
%% <p>
%% Please note: This will cause the TCP connection to the manager to be closed,
%% and the manager to be Tarted, if it supposed to do that according to it's
%% supervisor. If the manager event manager is used the server will be
%% Tarted, and the manager logged in again.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
logoff() ->
	Cmd = action("Logoff", []),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec mailbox_count(Mailbox::string()) -> {ok, MboxCount} | {error, Message}
%%                         MboxCount = record()
%%                         Message   = string()
%% @doc
%% Checks a voicemail account for new messages.
%% <p>
%% <em>Mailbox</em> is a complete mailbox ID e.g. 1234@default.
%% </p>
%% <p>
%% <pre>
%% -record(mbox_count,
%% {
%%     old_messages, % integer()
%%     new_messages  % integer()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
mailbox_count(Mailbox) ->
	Cmd = action("MailboxCount", [{"MailBox", Mailbox}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec mailbox_status(Mailbox::string()) -> {ok, Waiting} | {error, Message}
%%                                  Waiting = integer()
%%                                  Message = string()
%% @doc
%% Checks a voicemail account for status.
%% <p>
%% <em>Mailbox</em> is a complete mailbox ID e.g. 1234@default.
%% Returns the amount of waiting messages.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
mailbox_status(Mailbox) ->
	Cmd = action("MailboxStatus", [{"MailBox", Mailbox}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec monitor(Channel::string(), Mix::bool()) -> {Status, Message}
%%                                  Status  = ok | error
%%                                  Message = string()
%% @doc
%% Records the audio on a channel.
%% <p>
%% Records the audio on a channel to a file named the same as the channel, with
%% slashes changed into dashes. Mix tells whether the channels should be mixed
%% together when playback is finished.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
monitor(Channel, Mix) ->
	monitor(Channel, wav, Mix).

%% -----------------------------------------------------------------------------
%% @spec monitor(Channel::string(), Format, Mix::bool()) ->   {Status, Message}
%%               Format = atom() | string()
%%               Status  = ok | error
%%               Message = string()
%% @doc
%% Records the audio on a channel.
%% <p>
%% Works as {@link monitor/2} except that the file format will be
%% <em>Format</em>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
monitor(Channel, Format, Mix) ->
	Cmd = action("Monitor", [{"Channel", Channel},
	                         {"Format", Format},
	                         {"Mix", Mix}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec monitor(Channel::string(), FileName, Format, Mix::bool()) ->
%%                                                         {Status, Message}
%%               Format = atom() | string()
%%               Status  = ok | error
%%               Message = string()
%% @doc
%% Records the audio on a channel.
%% <p>
%% Works as {@link monitor/3} except the file will be named <em>FileName</em>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
monitor(Channel, FileName, Format, Mix) ->
	Cmd = action("Monitor", [{"Channel", Channel},
	                         {"FileName", FileName},
	                         {"Format", Format},
	                         {"Mix", Mix}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec originate(Channel::string(), Application::string(), Data::string(),
%%                 Timeout::integer(), CallerID::string(), Async::bool(),
%%                 Variables) -> {Status, Message}
%%
%%                 Priority = integer() | string() | atom()
%%                 Variables = [Var]
%%                 Var = {Name, Value}
%%                 Name = string()
%%                 Value = string() | integer() | atom()
%%                 Status = ok | error
%%                 Message = string()
%% @doc
%% Generates an outgoing call using an Application.
%% <p>
%% Generates a call to the <em>Channel</em>, using the <em>Application</em>
%% with <em>Data</em> as arguments. Gives up if the line has not been answered
%% before <em>Timeout</em>.
%% </p>
%% <p>
%% <em>Application</em> is a dialplan application.
%% If <em>Data</em> should hold more than one parameter, separate them with
%% <em>|</em>.
%% </p>
%% <p>
%% If <em>Async</em> is <code>false</code> the Asterisk Manager will block until
%% the origination has actually been picked up by the channel. The 
%% {@link ast_manager} Erlang module will <u>not</u> block, but many actions
%% might time out.
%% If Async is <code>true</code> the origination is asynchronous in the asterisk 
%% manager, and other events can be handled during the timeout. The calling
%% process will however be blocked until it has either succeeded or failed.
%% </p>
%% <p>
%% <em>CallerID</em> includes both the <em>caller_id_name</em> and
%% <em>caller_id</em> in the form <code>caller_id_name
%% &lt;caller_id&gt;</code>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
originate(Channel, Application, Data, Timeout, CallerID, Async, Variables) ->
	Cmd = action("Originate", [{"Channel", Channel},
	                           {"Application", Application},
	                           {"Data", Data},
	                           {"Timeout", Timeout},
	                           {"CallerID", CallerID},
	                           {"Async", Async}]),
	Pkg = add_vars(Cmd, Variables),
	case Async of
		true  -> send_event_response_cmd(Pkg);
		false -> send_cmd(Pkg, Timeout)
	end.

%% -----------------------------------------------------------------------------
%% @spec originate(Channel::string(), Context::string(), Exten, Priority,
%%                 Timeout::integer(), CallerID::string(), Async::bool(),
%%                 Variables) -> {Status, Message}
%%              Exten    = integer() | string() | atom()
%%              Priority = integer() | string() | atom()
%%              Variables = [Var]
%%              Var = {Name, Value}
%%              Name = string()
%%              Value = string() | integer() | atom()
%%              Status = ok | error
%%              Message = string()
%% @doc
%% Generates an outgoing call to a Extension, Context and Priority.
%% <p>
%% Generates a call to the <em>Context</em>, <em>Extention</em> and
%% <em>Priority</em>. Gives up if the line has not been answered
%% before <em>Timeout</em>.
%% </p>
%% <p>
%% If <em>Async</em> is <code>false</code> the Asterisk Manager will block until
%% the origination has actually been picked up by the channel. The 
%% {@link ast_manager} Erlang module will <u>not</u> block, but many actions
%% might time out.
%% If Async is <code>true</code> the origination is asynchronous in the asterisk 
%% manager, and other events can be handled during the timeout. The calling
%% process will however be blocked until it has either succeeded or failed.
%% </p>
%% <p>
%% <em>CallerID</em> includes both the <em>caller_id_name</em> and
%% <em>caller_id</em> in the form <code>caller_id_name
%% &lt;caller_id&gt;</code>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
originate(Channel, Context, Exten, Priority, Timeout,
                                         CallerID, Async, Variables) ->
	Cmd = action("Originate", [{"Channel", Channel},
	                           {"Context", Context},
	                           {"Exten", Exten},
	                           {"Priority", Priority},
	                           {"Timeout", Timeout},
	                           {"CallerID", CallerID},
	                           {"Async", Async}]),
	send_cmd(add_vars(Cmd, Variables), Timeout).

%% -----------------------------------------------------------------------------
%% @spec parked_calls() -> {ok, ParkedCalls} | {error, Message}
%%                         ParkedCalls = [ParkedCall]
%%                         ParkedCall = record()
%%                         Message = string()
%% @doc
%% List parked calls.
%% <p>
%% <pre>
%% -record(park_call,
%% {
%%     privilege,     % [atom()]
%%     exten,         % string()
%%     channel,       % string()
%%     from,          % string()
%%     timeout,       % integer()
%%     caller_id,     % string()
%%     caller_id_name % string()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
parked_calls() ->
	Cmd = action("ParkedCalls", []),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec ping() -> pong
%% @doc
%% Send a ping, receive a Pong.
%% <p>
%% Said to be a keep alive command by the Asterisk documentation but no
%% keep alive is really necessary for the manager interface.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
ping() ->
	Cmd = action("Ping", []),
	{pong, _} = send_cmd(Cmd),
	pong.

%% -----------------------------------------------------------------------------
%% @spec play_dtmf(Channel::string(), Digit::string()) -> mgr_response()
%% @doc
%% Plays a dtmf digit on the specified channel.
%% <p>
%% NOTE: Said to be in Asterisk 1.2.8 but is <u>not</u> in Asterisk 1.2.9.1.
%% This might not exist in any version of Asterisk and is <u>not</u> tested.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
play_dtmf(Channel, Digit) ->
	Cmd = action("PlayDTMF", [{"Channel", Channel}, {"Channel", Digit}]),         
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec queue_add(Queue, Interface, Penalty, Paused) -> mgr_response()
%% @doc
%% Add interface to queue.
%% @end
%% -----------------------------------------------------------------------------
queue_add(Queue, Interface, Penalty, Paused) ->
	Cmd = action("QueueAdd", [{"Queue", Queue},
	                          {"Interface", Interface},
	                          {"Penalty", Penalty},
	                          {"Paused", Paused}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec queue_pause(Queue::string(), Interface::string(), Paused::bool())
%%                                    -> {Status, Message} | {Status, Message}
%%                                        Status  = ok | error
%%                                        Message = string()
%% @doc
%% Makes a queue member temporarily unavailable or available again.
%% <p>
%% <em>Interface</em> is the queue member to be paused/made available  in
%% <em>Queue</em>.
%% In the queue member record the <em>Interface</em> is called
%% <code>location</code>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
queue_pause(Queue, Interface, Paused) ->
	Cmd = action("QueuePause", [{"Queue", Queue},
	                            {"Interface", Interface},
	                            {"Paused", Paused}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec queue_remove(Queue, Interface) -> mgr_response()
%% @doc
%% Remove an agent/interface from queue.
%% <p>
%% <em>Interface</em> is the queue member to be removed from <em>Queue</em>.
%% In the queue member record the <em>Interface</em> is called
%% <code>location</code>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
queue_remove(Queue, Interface) ->
	Cmd = action("QueueRemove", [{"Queue", Queue}, {"Interface", Interface}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec queues() -> mgr_response()
%% @doc
%% Lists queues.
%% <p>
%% NOTE: Does not work with Asterisk 1.2.9.1 or earlier.
%% Does not work with asterisk 1.2.10 either...
%% </p>
%% @end
%% -----------------------------------------------------------------------------
queues() ->
	Cmd = action("Queues", []),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec queue_status() ->  {ok, {QueueEntries, QueueMembers, QueueParams}}
%%                                QueueEntries = [QueueEntry]
%%                                QueueEntry   = record()
%%                                QueueMembers = [QueueMember]
%%                                QueueMember  = record()
%%                                QueueParams  = [QueueParams]
%%                                QueueParam   = record()
%% @doc
%% Returns Queue Status.
%% <p>
%% <pre>
%% -record(queue_entry,
%% {
%%     queue,          % string()
%%     position,       % integer()
%%     channel,        % string()
%%     caller_id,      % string()
%%     caller_id_name, % string()
%%     wait            % integer()
%% }).
%% </pre>
%% </p>
%% <p>
%% <pre>
%% -record(queue_member,
%% {
%%     privilege,   % [atom()]
%%     queue,       % string()
%%     location,    % string()
%%     membership,  % string()
%%     penalty,     % integer()
%%     calls_taken, % integer()
%%     last_call,   % integer()
%%     status,      % integer()
%%     paused       % bool()
%% }).
%% </pre>
%% </p>
%% <p>
%% <pre>
%% -record(queue_params,
%% {
%%     queue,              % string()
%%     max,                % integer()
%%     calls,              % integer()
%%     holdtime,           % integer()
%%     completed,          % integer()
%%     abandoned,          % integer()
%%     service_level,      % integer()
%%     service_level_perf, % number()
%%     weight              % integer()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
queue_status() ->
	Cmd = action("QueueStatus", []),
	{ok, List} = send_event_response_cmd(Cmd),
	{ok, format_queue_status(List, {[], [], []})}.

%% -----------------------------------------------------------------------------
%% @spec redirect(Channel::string(), Context::string(),
%%                Exten::string(), Priority) -> {Status, Message}
%%                Priority = string() | integer() | atom() 
%%                Status = ok | error
%%                Message = string()
%% @doc
%% Redirect/transfer a call.
%% <p>
%% Redirects <em>Channel</em>  to <em>Exten</em> in <em>Context</em> with
%% <em>Priority</em>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
redirect(Channel, Context, Exten, Priority) ->
	Cmd = action("Redirect", [{"Channel", Channel},
	                          {"Exten", Exten},
	                          {"Context", Context},
	                          {"Priority", Priority}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec redirect(Channel::string(), ExtraChannel::string(), Context::string(),
%%                Exten::string(), Priority) -> {Status, Message}
%%                Priority = string() | integer() | atom()
%%                Status = ok | error
%%                Message = string()
%% @doc
%% Redirect/transfer a call.
%% <p>
%% Redirects <em>Channel</em>  to <em>Exten</em> in <em>Context</em> with
%% <em>Priority</em>. <em>ExtraChannel</em> is the second call leg to transfer.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
redirect(Channel, ExtraChannel, Context, Exten, Priority) ->
	Cmd = action("Redirect", [{"Channel", Channel},
	                          {"ExtraChannel", ExtraChannel},
	                          {"Exten", Exten},
	                          {"Context", Context},
	                          {"Priority", Priority}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec set_cdr_user_field(Channel::string(), UserField, Append) ->
%%                                                          {Status, Message}
%%                          UserField = string() | integer() | atom()
%%                          Append    = bool()
%%                Status = ok | error
%%                Message = string()
%% @doc
%% Set the CDR user field.
%% @end
%% -----------------------------------------------------------------------------
set_cdr_user_field(Channel, UserField, Append) ->
	Cmd = action("SetCDRUserField", [{"Channel", Channel},
	                                 {"UserField", UserField},
	                                 {"Append", Append}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec set_var(Variable::string(), Value::string()) ->  {Status, Message}
%%                Status = ok | error
%%                Message = string()
%% @doc
%% Set a global variable.
%% @end
%% -----------------------------------------------------------------------------
set_var(Variable, Value) ->
	Cmd = action("Setvar", [{"Variable", Variable},
	                        {"Value", Value}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec set_var(Channel::string(), Variable::string(), Value::string()) ->
%%                                                      {Status, Message}
%%                Status = ok | error
%%                Message = string()
%% @doc
%% Set a local channel variable.
%% @end
%% -----------------------------------------------------------------------------
set_var(Channel, Variable, Value) ->
	Cmd = action("Setvar", [{"Channel", Channel},
	                        {"Variable", Variable},
	                        {"Value", Value}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec sip_peers() -> {ok, PeerList}
%%                      PeerList = [PeerInfo]
%%                      PeerInfo = record()
%% @doc
%% Returns SIP peers status.
%% <p>
%% <pre>
%% -record(sip_peer,
%% {
%%     channeltype,      % string()
%%     object_name,      % string()
%%     chan_object_type, % atom() peer | user
%%     ip_address,       % string()
%%     ip_port,          % integer()
%%     dynamic,          % bool()
%%     nat_support,      % bool()
%%     acl,              % bool()
%%     status            % string()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
sip_peers() ->
	Cmd = action("SIPPeers", []),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec iax_peers() -> {ok, IAXData}
%%                      IAXData = record()
%% @doc
%% Returns IAX peers status.
%% <p>
%% <pre>
%% -record(iax_peers,
%% 	{
%% 	  data,           % [#iax_peer{}]
%% 	  summary         % string()
%% 	}).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------

iax_peers() ->
    Cmd = action("Iaxpeers", []),
    send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec sip_show_peer(Peer::string()) -> {ok, PeerDetails} | {error, Message}
%%                      PeerDetails = record()
%%                      Message = string()
%% @doc
%% Show one SIP peer with details on current status.
%% NOTE! Using this function will cause a crash due to bug in Asterisk!
%% <p>
%% <pre>
%% -record(sip_peer_details,
%% {
%%     channeltype,        % string()
%%     object_name,        % string()
%%     chan_object_type,   % string()
%%     secret_exists,      % bool()
%%     md5_secret_exists,  % bool()
%%     context,            % string()
%%     language,           % string()
%%     ama_flags,          % string()
%%     cid_calling_pres,   % integer()
%%     callgroup,          % string()
%%     pickupgroup,        % string()
%%     voice_mailbox,      % string()
%%     last_msg_sent,      % integer()
%%     call_limit,         % integer()
%%     dynamic,            % bool()
%%     caller_id,          % string()
%%     reg_expire,         % integer()
%%     sip_auth_insecure,  % atom() port, invite, port_invite, no
%%     sip_nat_support,    % atom() No | Route | Always | RFC3581 | Unknown
%%     acl,                % bool()
%%     sip_can_reinvite,   % bool()
%%     sip_primisc_redir,  % bool()
%%     sip_user_phone,     % bool()
%%     sip_dtmf_mode,      % atom() rfc2833 | info | inband | auto
%%     sip_last_msg,       % integer()
%%     to_host,            % string()
%%     ip_address,         % string()
%%     ip_port,            % integer()
%%     default_ip_address, % string()
%%     default_ip_port,    % integer()
%%     default_user_name,  % string()
%%     codecs,             % {Identifier, Names},
%%         Names = [Name], Name = atom()
%%     codec_order,        % Names
%%     status,             % string()
%%     sip_user_agent,     % string()
%%     reg_contact         % string()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
sip_show_peer(Peer) ->
	Cmd = action("SIPShowPeer", [{"Peer", Peer}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec status() -> {ok, Channels}
%%       Channels = [ChannelStatus]
%%       ChannelStatus = record()
%% @doc
%% Returns status for channels.
%% <p>
%% <pre>
%% -record(channel_status,
%% {
%%     privilege,      % [atom()]
%%     channel,        % string()
%%     caller_id,      % string()
%%     caller_id_name, % string()
%%     account,        % string()
%%     state,          % atom(),
%%         'Up' | 'Ringing' | 'Busy' | 'Down' | 'Rsrvd' | 
%%         'OffHook' | 'Dialing' | 'Ring'
%%     context,        % string()
%%     extension,      % string()
%%     priority,       % string()
%%     seconds,        % integer()
%%     link,           % string()
%%     unique_id       % string()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
status() ->
	Cmd = action("Status", []),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec status(Channel::string()) -> {ok, ChannelStatus} | {error, Message}
%%               ChannelStatus = record()
%%               Message = string()
%% @doc
%% Returns channel status.
%% <p>
%% <pre>
%% -record(channel_status,
%% {
%%     privilege,      % [atom()]
%%     channel,        % string()
%%     caller_id,      % string()
%%     caller_id_name, % string()
%%     account,        % string()
%%     state,          % atom(),
%%         'Up' | 'Ringing' | 'Busy' | 'Down' | 'Rsrvd' | 
%%         'OffHook' | 'Dialing' | 'Ring'
%%     context,        % string()
%%     extension,      % string()
%%     priority,       % string()
%%     seconds,        % integer()
%%     link,           % string()
%%     unique_id       % string()
%% }).
%% </pre>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
status(Channel) ->
	Cmd = action("Status", [{"Channel", Channel}]),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec stop_monitor(Channel::string()) -> {Status, Message}
%% @doc
%% Stops monitoring a channel.
%% <p>
%% The <em>Channel</em> must be monitored with the {@link monitor/4} command.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
stop_monitor(Channel) ->
	Cmd = action("StopMonitor", [{"Channel", Channel}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec zap_dial_off_hook(ZapChannel::integer(), Number::string()) ->
%%                                                               mgr_response()
%% @doc
%% Dial over Zap channel while offhook.
%% @end
%% -----------------------------------------------------------------------------
zap_dial_off_hook(ZapChannel, Number) ->
	Cmd = action("ZapDialOffhook", [{"ZapChannel", ZapChannel},
	                                {"Number", Number}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec zap_dnd_off(ZapChannel::integer()) -> mgr_response()
%% @doc
%% Toggle Zap channel Do Not Disturb status OFF
%% @end
%% -----------------------------------------------------------------------------
zap_dnd_off(ZapChannel) ->
	Cmd = action("ZapDNDoff", [{"ZapChannel", ZapChannel}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec zap_dnd_on(ZapChannel::integer()) -> mgr_response()
%% @doc
%% Toggle Zap channel Do Not Disturb status OFF
%% @end
%% -----------------------------------------------------------------------------
zap_dnd_on(ZapChannel) ->
	Cmd = action("ZapDNDon", [{"ZapChannel", ZapChannel}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec zap_hangup(ZapChannel::integer()) -> mgr_response()
%% @doc
%% Hangup a Zap channel
%% @end
%% -----------------------------------------------------------------------------
zap_hangup(ZapChannel) ->
	Cmd = action("ZapHangup", [{"ZapChannel", ZapChannel}]),
	send_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec zap_show_channels() -> mgr_response()
%% @doc
%% Show status of zapata channels.
%% @end
%% -----------------------------------------------------------------------------
zap_show_channels() ->
	Cmd = action("ZapShowChannels", []),
	send_event_response_cmd(Cmd).

%% -----------------------------------------------------------------------------
%% @spec zap_transfer(ZapChannel::integer()) -> mgr_response()
%% @doc
%% Transfer a Zap Channel
%% @end
%% FIXME: does this take no more arguments?
%% -----------------------------------------------------------------------------
zap_transfer(ZapChannel) ->
	Cmd = action("ZapTransfer", [{"ZapChannel", ZapChannel}]),
	send_cmd(Cmd).

%%% ----------------------------------------------------------------------------
%%%                        gen_server callbacks
%%% ----------------------------------------------------------------------------

%% @hidden
init({Callback, Host, Port, Args}) ->
	{ok, CBState} = Callback:init(Args),
	TCPOpts = [binary, {active, true}, {packet, line}],
	case gen_tcp:connect(Host, Port, TCPOpts) of
		{ok, Socket} ->
			RTTid = ets:new(reply, [set, protected, {keypos, 1}]),
			PKGTid = ets:new(pkgs, [bag, protected, {keypos, 1}]),
			{ok, #state{socket = Socket,
						callback = Callback,
						callback_state = CBState,
						reply_tbl = RTTid,
						pkg_tbl = PKGTid,
						pkg_acc = []}};
		Error ->
			% ugly hack to not make it hammer with connection attempts
			timer:sleep(1000),
			exit(Error)
	end.

%% @hidden
handle_cast(Request, State) ->
	{stop, {unhandled_cast, Request}, State}.

%% @hidden
handle_call({'__send', Pkg}, From, State) ->
	Id = action_id(),
	gen_tcp:send(State#state.socket, add_action_id(Pkg, Id)),
	ets:insert(State#state.reply_tbl, {Id, From}), % remember who to respond to
	{noreply, State};
handle_call({'__send_async', Pkg}, _From, State) ->
	gen_tcp:send(State#state.socket, add_action_id(Pkg, action_id())),
	{reply, ok, State};
handle_call({'__send_event_resp', Pkg}, From, State) ->
	% this need to be kept in the table
	Id = action_id(),
	gen_tcp:send(State#state.socket, add_action_id(Pkg, Id)),
	ets:insert(State#state.reply_tbl, {Id, From, keep}), 
	{noreply, State};
handle_call('__stop', _From, State) ->
	{stop, normal, ok, State};
handle_call(Request, From, State) ->
	{stop, {unhandled_call, {Request, From}}, State}.

%% @hidden
handle_info({tcp, _Socket, <<"\r\n">>}, State) ->
    case State#state.pkg_acc of
	[] ->
	    {noreply, State};
	_ ->
	    Pkg = ast_manager_parser:parse_package(lists:reverse(State#state.pkg_acc)),
	    case Pkg of
		[] ->
		    {noreply, State};
		Pkg ->
		    case handle_mgr_package(State, Pkg) of
			{ok, NewCBState} ->
			    {noreply, State#state{callback_state = NewCBState,
						  pkg_acc = []}};
			{stop, Reason, NewCBState} ->
			    {stop, Reason, State#state{callback = NewCBState, pkg_acc = []}}
		    end
	    end
    end;

handle_info({tcp, _Socket, <<"\n">>}, State) ->
		{noreply, State};
handle_info({tcp, _Socket, Data}, State) ->
	Line = strip_nl(Data),
	{noreply, State#state{pkg_acc = [Line | State#state.pkg_acc]}};
handle_info({tcp_closed, Socket}, State) ->
	{stop, {tcp_closed, Socket}, State};
handle_info({tcp_error, Socket, Reason}, State) ->
	{stop, {tcp_error, Socket, Reason}, State};
handle_info(Info, State) ->
	{stop, {unhandled_info, Info}, State}.

%% @hidden
terminate(Reason = {tcp_error, _Socket, _TCPReason}, State) ->
	Callback = State#state.callback,
	Callback:terminate(Reason, State#state.callback_state);
terminate(Reason = {tcp_closed, _Socket}, State) ->
	Callback = State#state.callback,
	Callback:terminate(Reason, State#state.callback_state);
terminate(Reason, State) ->
	Callback = State#state.callback,
	Callback:terminate(Reason, State#state.callback_state),
	gen_tcp:close(State#state.socket).

%% @hidden
code_change(OldVsn, State, Extra) ->
	Callback = State#state.callback,
	CBState = State#state.callback_state,
	{ok, NewCBState} = Callback:code_change(OldVsn, CBState, Extra),
	{ok, State#state{callback_state = NewCBState}}.

%%% ----------------------------------------------------------------------------
%%%                           Internal functions
%%% ----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @spec handle_mgr_package(State::record(), Package) -> 
%%                {ok, NewCBState} | {stop, Reason, NewCBState}
%%                Package = {Type::atom(), ID::atom(), Vars}
%%                Vars = [Var]
%%                Var = {ID::atom(), Value}
%%                Value = string()
%%                NewCBState = term()
%%                Reason = term()
%% @doc
%% Reads a Asterisk Manager package and decides what to do with it.
%% @end
%% -----------------------------------------------------------------------------
handle_mgr_package(#state{callback = Callback, callback_state = CBState},
	                                               {event, ID, Record, nil}) ->
	% must return {ok, NewCBState} or {stop, Reason, NewCBState}
	Callback:handle_event({ID, Record}, CBState);
handle_mgr_package(State, {event, ID, Record, ActionID}) ->
	case ID of % some return values will include more than one package
		'OriginateFailure' ->
			ets:insert(State#state.pkg_tbl, {ActionID, Record}),
			send_reply(State#state.reply_tbl,
				State#state.pkg_tbl, ActionID);
		'OriginateSuccess' ->
			ets:insert(State#state.pkg_tbl, {ActionID, Record}),
			send_reply(State#state.reply_tbl,
				State#state.pkg_tbl, ActionID);
		'StatusComplete' ->
			send_reply(State#state.reply_tbl,
					   State#state.pkg_tbl, ActionID);
		'ParkedCallsComplete' ->
			send_reply(State#state.reply_tbl,
					   State#state.pkg_tbl, ActionID);
		'PeerlistComplete' ->
			send_reply(State#state.reply_tbl,
					   State#state.pkg_tbl, ActionID);
		'QueueStatusComplete' ->
			send_reply(State#state.reply_tbl,
					   State#state.pkg_tbl, ActionID);
		'DBGetResponse' -> % this is only one package
			[{_, To, keep}] = ets:lookup(State#state.reply_tbl,
										 ActionID),
			gen_server:reply(To, {ok, Record});
		'ZapShowChannelsComplete' ->
			send_reply(State#state.reply_tbl,
					   State#state.pkg_tbl, ActionID);
		'AgentsComplete' ->
			send_reply(State#state.reply_tbl,
					   State#state.pkg_tbl, ActionID);
		_Else -> % collect package to return all of them together
			ets:insert(State#state.pkg_tbl,
					   {ActionID, Record})
	end,
	{ok, State#state.callback_state};
handle_mgr_package(State, {response, Status, Param, ActionID}) ->
	Tid = State#state.reply_tbl,
	case ets:lookup(Tid, ActionID) of
		[{ActionID, Receiver}] -> % normal package
			ets:delete(Tid, ActionID),
			gen_server:reply(Receiver, {erlang_status(Status), Param});
		[{ActionID, Receiver, keep}] -> % the receiver might need to be kept
			case Status of
				'Error' ->
					% there wont be any more packages for this action
					ets:delete(Tid, ActionID),
					gen_server:reply(Receiver, {error , Param});
				'Success' ->
					% more packages will come, those will be returned later
					ok 
			end;
		[] ->
			exit({unknown_action_id, ActionID, Param})
	end,
	{ok, State#state.callback_state};
handle_mgr_package(State, {error, unhandled_package}) ->
	error_logger:error_report("Unhandled/empty package"),
	{ok, State#state.callback_state}.

%% -----------------------------------------------------------------------------
%% @spec strip_nl(Binary::binary()) -> binary()
%% @doc
%% Strips newline characters from a binary, either \r\n or \n
%% @end
%% -----------------------------------------------------------------------------
strip_nl(Binary) ->
	Size = size(Binary),
	NLSize = Size - 1,
	CRNLSize = Size - 2,
	case Binary of
		<<Bytes:CRNLSize/binary, "\r\n">> ->
			Bytes;
		<<Bytes:NLSize/binary, "\n">> ->
			Bytes
	end.

%% -----------------------------------------------------------------------------
%% @spec send_cmd(Cmd) -> mgr_response()
%% @doc
%% Sends a command package to Asterisk.
%% @end
%% -----------------------------------------------------------------------------
send_cmd(Cmd) ->
	gen_server:call(?NAME, {'__send', Cmd}).


%% -----------------------------------------------------------------------------
%% @spec send_cmd(Cmd::string(), Timeout::integer()) -> mgr_response()
%% @doc
%% Sends a command package to Asterisk, but changes the default timeout of the
%% gen_server.
%% @end
%% -----------------------------------------------------------------------------
send_cmd(Cmd, Timeout) ->
	gen_server:call(?NAME, {'__send', Cmd}, Timeout + 500).

%% -----------------------------------------------------------------------------
%% @spec send_async_cmd(Cmd::string()) -> ok
%% @doc
%% Sends an asynchronous command to Asterisk.
%% An asynchronous command is one that does not return anything.
%% @end
%% -----------------------------------------------------------------------------
send_async_cmd(Cmd) ->
	gen_server:call(?NAME, {'__send_async', Cmd}).

%% -----------------------------------------------------------------------------
%% @spec send_event_response_cmd(Cmd) -> mgr_response()
%% @doc
%% Send a command that gives the reply as a series of events.
%% @end
%% -----------------------------------------------------------------------------
send_event_response_cmd(Cmd) ->
	gen_server:call(?NAME, {'__send_event_resp', Cmd}, 20000).

%% -----------------------------------------------------------------------------
%% @spec send_reply(ReplyTid, PkgTid, ActionID) -> ok
%% @doc
%% Sending a reply to commands where the result is given as event packages.
%% @end
%% -----------------------------------------------------------------------------
send_reply(ReplyTid, PkgTid, ActionID) ->
	[{ActionID, To, keep}] = ets:lookup(ReplyTid, ActionID),
	Pkgs = lists:foldl(fun(Element, Acc) -> % don't want the key from ETS
		                   {_ActionID, Pkg} = Element,
						   [Pkg | Acc]
	                   end, [], ets:lookup(PkgTid, ActionID)),
	ets:delete(ReplyTid, ActionID),
	ets:delete(PkgTid, ActionID),
	gen_server:reply(To, {ok, Pkgs}).

%% -----------------------------------------------------------------------------
%% @spec action_id() -> integer()
%% @doc
%% Creates a relatively unique ID.
%% The IDs can occur every 1000 000 second.
%% @end
%% -----------------------------------------------------------------------------
action_id() ->
	{_Me, S, Mi} = now(),
	(S * 1000000) + Mi.

%% -----------------------------------------------------------------------------
%% @spec add_action_id(Pkg::string(), Id::integer()) -> string()
%% @doc
%% Adds the Action ID to a action before sending. Also adds the "extra" \r\n to
%% make the package complete.
%% @end
%% NOTE: If I would add these after the first \r\n this might be faster,
%% BUT, we also need to add the final \r\n to the end of the string :/
%% -----------------------------------------------------------------------------
add_action_id(Pkg, Id) ->
	Pkg ++ io_lib:format("ActionID: ~B\r\n\r\n", [Id]).

%% -----------------------------------------------------------------------------
%% @spec action(Action::string(), Args) -> mgr_response()
%%              Args = [Arg]
%%              Arg  = {Name, Value}
%%              Name = string()
%%              Value = string() | atom() | integer()
%% @doc
%% Creates a package to be sent a command to Asterisk.
%% @end
%% -----------------------------------------------------------------------------
action(Action, Args) ->
	io_lib:format("Action: ~s\r\n", [Action]) ++ args([], Args).
	

%% -----------------------------------------------------------------------------
%% @spec args(Pkg::string(), Args) -> string()
%%                Args = [Arg]
%%                Arg  = {Name, Value}
%%                Name = string()
%%                Value = string() | atom() | integer()
%% @doc
%% Makes a string/package of arguments.
%% @end
%% -----------------------------------------------------------------------------
args(Pkg, [{Name, Value}|Args]) when is_list(Value) ->
	args(io_lib:format("~s: ~s\r\n", [Name, Value]) ++ Pkg, Args);
args(Pkg, [{Name, Value}|Args]) when is_integer(Value) ->
	args(io_lib:format("~s: ~B\r\n", [Name, Value]) ++ Pkg, Args);
args(Pkg, [{Name, Value}|Args]) when is_atom(Value) ->
	args(io_lib:format("~s: ~p\r\n", [Name, Value]) ++ Pkg, Args);
args(Pkg, []) ->
	Pkg.

%% -----------------------------------------------------------------------------
%% @spec add_vars(Pkg::string(), Vars::list()) -> string()
%% @doc
%% Adds variables to a package.
%% @end
%% -----------------------------------------------------------------------------
add_vars(Pkg, [{Name, Value}| Vars]) when is_list(Value) ->
	add_vars(Pkg ++ io_lib:format("Variable: ~s=~s\r\n", [Name, Value]), Vars);
add_vars(Pkg, [{Name, Value}| Vars]) when is_integer(Value) ->
	add_vars(Pkg ++ io_lib:format("Variable: ~s=~B\r\n", [Name, Value]), Vars);
add_vars(Pkg, [{Name, Value}| Vars]) when is_atom(Value) ->
	add_vars(Pkg ++ io_lib:format("Variable: ~s=~p\r\n", [Name, Value]), Vars);
add_vars(Pkg, []) ->
	Pkg.

%% -----------------------------------------------------------------------------
%% @spec erlang_status(Status::tuple()) -> mgr_response()
%% @doc
%% Turns the AMI status messages in to more "Erlang like" status.
%% <p>
%% E.g. <code>'Success' -> ok</code>.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
erlang_status('Success') ->
	ok;
erlang_status('Error') ->
	error;
erlang_status('Events Off') ->
	ok;
erlang_status('Events On') ->
	ok;
erlang_status('Pong') ->
	pong;
erlang_status('Goodbye') ->
	ok;
erlang_status('Follows') ->
	ok.

%% -----------------------------------------------------------------------------
%% @spec event_flags(List::list()) -> string() | atom()
%% @doc
%% Creates either a string of flags or returns the only element in the list.
%% @end
%% -----------------------------------------------------------------------------
event_flags([A]) ->
	A;
event_flags([A, B]) when is_atom(A), is_atom(B) ->
	io_lib:format("~w,~w", [A, B]);
event_flags([A, B]) when is_list(A), is_list(B) ->
	io_lib:format("~s,~s", [A, B]);
event_flags([A, B, C]) when is_atom(A), is_atom(B), is_atom(C) ->
	io_lib:format("~w,~w,~w", [A, B, C]);
event_flags([A, B, C]) when is_list(A), is_list(B), is_list(C) ->
	io_lib:format("~s,~s,~s", [A, B, C]).

format_queue_status([Record = #queue_entry{}|T],
		{QueueEntries, QueueMembers, QueueParams}) ->
	format_queue_status(T, {[Record|QueueEntries], QueueMembers, QueueParams});
format_queue_status([Record = #queue_member{}|T],
		{QueueEntries, QueueMembers, QueueParams})->
	format_queue_status(T, {QueueEntries, [Record|QueueMembers], QueueParams});
format_queue_status([Record = #queue_params{}|T],
		{QueueEntries, QueueMembers, QueueParams}) ->
	format_queue_status(T, {QueueEntries, QueueMembers, [Record|QueueParams]});
format_queue_status([], Accs) ->
	Accs.

%% -----------------------------------------------------------------------------
%% @spec hex(B::binary())-> string()
%% @doc
%% Turns a binary md5 digest in to a string of hex digits.
%% <p>
%% Code stolen form Erlang mailing list
%% Author Vladimir Sekissov &lt;svg@@surnet.ru&gt;
%% </p>
%% @end
%% -----------------------------------------------------------------------------
hex(B) when is_binary(B) ->
	hex(binary_to_list(B));
hex(L) when list (L) ->
  lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
  [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I) ->
	[$0, hex0(I)].

%% -----------------------------------------------------------------------------
%% @spec hex0(I::integer()) -> integer()
%% @doc
%% Changes an integer into a hex character.
%% Code stolen form Erlang mailing list
%% Author: Vladimir Sekissov &lt;svg@@surnet.ru&gt;
%% @end
%% -----------------------------------------------------------------------------
hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 +I. 

%%% ----------------------------------------------------------------------------
%%%                       Type definitions
%%%
%%% @type address() = string() | atom() | ip_address().
%%% @type ip_adrress(). See inet(3).
%%% @type mgr_event() = {Id, Params}
%%%                     Id = atom()
%%%                     Params = [Param]
%%%                     Param = {Name, Value}
%%%                     Name = atom()
%%%                     Value = term().
%%%
%%% @type mgr_response() = ok | {ok, Params} | {error, Params}
%%%                        Params = [Param]
%%%                        Param = {Name, Value}
%%%                        Name = atom()
%%%                        Value = string().
%%%                        
%%% Params usually includes <code>{'Message', string()}</code>
%%% @end
%%% ----------------------------------------------------------------------------
