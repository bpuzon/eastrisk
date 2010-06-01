%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.3, 2007-05-16
%%% @copyright 2006 Erlang Training and Consulting
%%% @doc
%%% Records for the Asterisk Manager interface.
%%% @end
%%%
%%% IAX records added 2009-11-13
%%% ----------------------------------------------------------------------------
-record(agent,
        {
         id,           % integer()
         name,         % string()
         status,       % atom()
         channel,      % string()
         time,         % integer()
         talking_to    % string()
        }).

-record(exten_state,
        {
         privilege,
         status,   % integer()
         hint,     % string()
         context,  % string()
         exten     % string()
        }).  

-record(mbox_count,
        {
         old_messages, % integer()
         new_messages % integer()
        }).

-record(park_call,
        {
		 privilege,     % [atom()]
         exten,         % string()
         channel,       % string()
         from,          % string()
         timeout,       % integer()
         caller_id,     % string()
         caller_id_name % string()
        }).

-record(queue_entry,
        {
         privilege,      % [atom()]
         queue,          % string()
         position,       % integer()
         channel,        % string()
         caller_id,      % string()
         caller_id_name, % string()
         wait,           % integer()
		 count           % integer()
        }).

-record(queue_member,
        {
         privilege,   % [atom()]
         queue,       % string()
         location,    % string()
         membership,  % string()
         penalty,     % integer()
         calls_taken, % integer()
         last_call,   % integer()
         status,      % integer()
         paused       % bool()
        }).

-record(queue_params,
        {
         queue,              % string()
         max,                % integer()
         calls,              % integer()
         holdtime,           % integer()
         completed,          % integer()
         abandoned,          % integer()
         service_level,      % integer()
         service_level_perf, % number()
         weight              % integer()
        }).

-record(sip_peer,
        {
         channeltype,      % string()
         object_name,      % string()
         chan_object_type, % atom() peer | user
         ip_address,       % string()
         ip_port,          % integer()
         dynamic,          % bool()
         nat_support,      % bool()
         acl,              % bool()
         status            % string()
        }).

-record(iax_peers,
	{
	  data,           % list()
	  summary         % string()
	 }).

-record(iax_peer,
	{
	  object_name,    % string(),
	  ip_address,     % string(),
	  ip_mask,        % string(),
	  ip_port,        % integer(),
	  status          % string()
	 }).


-record(sip_peer_details,
        {
         channeltype,        % string()
         object_name,        % string()
         chan_object_type,   % string()
         secret_exists,      % bool()
         md5_secret_exists,  % bool()
         context,            % string()
         language,           % string()
         ama_flags,          % string()
         cid_calling_pres,   % integer()
         callgroup,          % string()
         pickupgroup,        % string()
         voice_mailbox,      % string()
         last_msg_sent,      % integer()
         call_limit,         % integer()
         dynamic,            % bool()
         caller_id,          % string()
         reg_expire,         % integer()
         sip_auth_insecure,  % atom() port, invite, port_invite, no
         sip_nat_support,    % atom() No | Route | Always | RFC3581 | Unknown
         acl,                % bool()
         sip_can_reinvite,   % bool()
         sip_primisc_redir,  % bool()
         sip_user_phone,     % bool()
         sip_dtmf_mode,      % atom() rfc2833 | info | inband | auto
         sip_last_msg,       % integer()
         to_host,            % string()
         ip_address,         % string()
         ip_port,            % integer()
         default_ip_address, % string()
         default_ip_port,    % integer()
         default_user_name,  % string()
         codecs,             % tuple, {Identifier, Names} Names = [Name] Name = atom()
         codec_order,        % Names
         status,             % string()
         sip_user_agent,     % string()
         reg_contact         % string()
        }).

-record(channel_status,
        {
         privilege,      % [atom()]
         channel,        % string()
         caller_id,      % string()
         caller_id_name, % string()
         account,        % string()
         state,          % atom(), 'Up' | 'Ringing' | 'Busy' | 'Down' | 'Rsrvd' | 'OffHook' | 'Dialing' | 'Ring'
         context,        % string()
         extension,      % string()
         priority,       % string()
         seconds,        % integer()
         link,           % string()
         unique_id       % string()
        }).

-record(zap_channel,
        {
         channel,      % integer()
         signalling,   % string()
         context,      % string()
         dnd,          % boolean()
         alarm         % string()
        }).

-record(agent_event,
{
	privilege,         % [atom()]
	agent_id,          % integer()
	login_chan,        % string()
	login_time         % integer()
}).

%%% ===========================================================================
%%%                               Event records
%%% ===========================================================================

-record(shutdown,
{
	privilege,         % [atom()]
	shutdown,          % 'Uncleanly' | 'Cleanly'
	restart            % bool()
}).

-record(caller_id,
{
	privilege,         % [atom()]
	channel,           % string()
	caller_id,         % string()
	caller_id_name,    % string()
	unique_id,         % string()
	cid_calling_pres   % integer()
}).

-record(channel,
{
	privilege,         % [atom()]
	event,             % string()
	channel,           % string()
	state,             % atom()
	caller_id,         % string()
	caller_id_name,    % string()
	caller_id_num,     % string()
	unique_id          % string()
}).

-record(exten,
{
	privilege,         % [atom()]
	channel,           % string()
	context,           % string()
	exten,             % string()
	priority,          % string()
	application,       % string()
	app_data,          % string()
	unique_id          % string()
}).

-record(ast_state,
{
	privilege,         % [atom()]
	channel,           % string()
	state,             % atom()
	caller_id,         % string()
	caller_id_name,    % string()
	unique_id          % string()
}).

-record(hangup,
{
	privilege,         % [atom()]
	channel,           % string()
	cause,             % integer()
	cause_txt,         % string()
	unique_id          % string()
}).

-record(peer_status,
{
	privilege,         % [atom()]
	peer,              % string()
	peer_status        % atom()
}).

-record(reload,
{
	privilege,         % [atom()]
	message            % string()
}).

-record(link,
{
	privilege,         % [atom()]
	channel_1,         % string()
	channel_2,         % string()
	unique_id_1,       % string()
	unique_id_2,       % string()
	caller_id_1,       % string()
	caller_id_2        % string()
}).

-record(call,
{
	privilege,                % [atom()]
	source,                   % string()
	destination,              % string()
	caller_id,                % string()
	caller_id_name,           % string()
	src_unique_id,            % string()
	dest_unique_id           % string()
}).

-record(rename,
{
	privilege,
	old_name,
	new_name,
	unique_id
}).

-record(alarm,
{
	privilege,
	alarm,            % string()
	channel           % integer()
}).

-record(dnd_state,
{
	privilege,
	channel,          % string()
	status            % enabled | disabled
}).

-record(hold,
{
	privilege,
	channel, 
	unique_id
}).

-record(registry,
{
	privilege,
	channel,
	username,
	domain,
	status         % atom()
}).

-record(log,
{
	privilege,
	channel,
	enabled,        % bool()
	reason          % string()
}).

-record(agent_call,
{
	privilege,
	agent,
	channel,
	caller_id,
	caller_id_name,
	context,
	extension,
	priority
}).

-record(queue_event,
{
	privilege,
	queue,
	channel,
	member,
	unique_id,      % string()
	hold_time,
	talk_time,
	reason          % agent | caller
}).

-record(voicemail,
{
	privilege,    % [atom()]
	mailbox,      % string()
	waiting,      % integer()
	new,          % integer()
	old           % integer()
}).

-record(originate,
{
	privilege,
	channel,
	context,
	exten,
	reason,
	unique_id
}).
