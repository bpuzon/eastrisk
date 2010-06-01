%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%% @private
%%% @version 0.3, 2007-05-16
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% Parser for manager events and responses.
%%% @end
%%% ----------------------------------------------------------------------------
-module(ast_manager_parser).

-export([
	parse_package/1
        ]).

-include("ast_mgr.hrl").

-define(IAX_RE, "(\\S+)\\s+(\\S+)\\s+\\(.\\)\\s+(\\S+)\\s+(\\S+)(\\s+\\(.\\))?\\s+((OK .*)|(\\S+))").
-define(warning(S), io:format("WARNING: ~p\n",[lists:flatten(S)])).
%-define(warning, ok).

%% -----------------------------------------------------------------------------
%% @spec parse_package(Package::List) ->
%%          {Record, ActionID}
%%           List = [binary()]
%%           Record = record()
%%           ActionID = integer()
%% @doc
%% Parses a Asterisk Manager package.
%% @end
%% -----------------------------------------------------------------------------
parse_package([<<"Event: ", Event/binary>> | Package]) ->
	EventName = list_to_atom(binary_to_list(Event)),
	{Record, ActionID} = parse_event(EventName, Package),
	{event, EventName, Record, ActionID};
parse_package([<<"Response: ", Status/binary>> | Package]) ->
	{Param, ActionID} = parse_response(Package, {nil, nil}),
	{response, binary_to_atom(Status), Param, ActionID};
parse_package([<<"Asterisk Call Manager", _/binary>>|Package]) ->
	% this line is always sent before the first package
	parse_package(Package); 
parse_package([<<"ActionID: ", ActionId/binary>> | Package]) ->
    %% iaxpeers command begins with an ActionId
    Param = parse_iaxlist(Package),
    {response, 'Success', Param, list_to_integer(binary_to_list(ActionId))};
parse_package([Ignore|Package]) ->
	error_logger:info_report(["Warning, one line ignored",
		{"Badly formated line", binary_to_list(Ignore)}]),
	parse_package(Package);
parse_package([]) ->
	exit(unhandler_or_empty_manager_package).

parse_response([<<"ActionID: ", A/binary>>|T], {Param, _}) ->
	parse_response(T, {Param, binary_to_integer(A)}); 
parse_response([<<"Message: Mailbox Message Count">>|T], {_, ActionID}) ->
	mailbox_count_record(T, #mbox_count{}, ActionID);
parse_response([<<"Message: Extension Status">>|T], {_, ActionID}) ->
	exten_state_record(T, #exten_state{}, ActionID);
parse_response([<<"Message: ", M/binary>>|T], {_, ActionID}) ->
	parse_response(T, {binary_to_list(M), ActionID});
parse_response([<<"Value: ", V/binary>>|T], {_, ActionID}) ->
	parse_response(T, {binary_to_list(V), ActionID});
parse_response([<<"Challenge: ", C/binary>>|T], {_, ActionID}) ->
	parse_response(T, {binary_to_list(C), ActionID});
parse_response([<<"Waiting: ", W/binary>>|T], {_, ActionID}) ->
	parse_response(T, {binary_to_integer(W), ActionID});
parse_response([<<"Mailbox: ", _/binary>>|T], Acc) ->
	parse_response(T, Acc);
parse_response([<<"Variable: ", _/binary>>|T], Acc) ->
	parse_response(T, Acc);
parse_response([], Acc) ->
	Acc.

%%% ============================================================================
%%%                      Events not triggered by actions
%%% ============================================================================
parse_event('Alarm', Elements) ->
	alarm_record(Elements, #alarm{}, nil);
parse_event('AlarmClear', Elements) ->
	alarm_record(Elements, #alarm{}, nil);
parse_event('LogChannel', Elements) ->
	log_record(Elements, #log{}, nil);
parse_event('DNDState', Elements) ->
	dnd_state_record(Elements, #dnd_state{}, nil);
parse_event('Join', Elements) ->
	queue_entry_record(Elements, #queue_entry{}, nil);
parse_event('Leave', Elements) ->
	queue_entry_record(Elements, #queue_entry{}, nil);
parse_event('Registry', Elements) ->
	registry_record(Elements, #registry{}, nil);
parse_event('Hold', Elements) ->
	hold_record(Elements, #hold{}, nil);
parse_event('Unhold', Elements) ->
	hold_record(Elements, #hold{}, nil);
parse_event('Hangup', Elements) ->
	hangup_record(Elements, #hangup{}, nil);
parse_event('OriginateSuccess', Elements) ->
	originate_record(Elements, #originate{}, nil);
parse_event('OriginateFailure', Elements) ->
	originate_record(Elements, #originate{}, nil);
parse_event('Link', Elements) ->
	link_record(Elements, #link{}, nil);
parse_event('Unlink', Elements) ->
	link_record(Elements, #link{}, nil);
parse_event('Newexten', Elements) ->
	exten_record(Elements, #exten{}, nil);
parse_event('Newstate', Elements) ->
	ast_state_record(Elements, #ast_state{}, nil);
parse_event('ExtensionStatus', Elements) ->
	exten_state_record(Elements, #exten_state{}, nil);
parse_event('Rename', Elements) ->
	rename_record(Elements, #rename{}, nil);
parse_event('Newchannel', Elements) ->
	channel_record(Elements, #channel{}, nil);
parse_event('Newcallerid', Elements) ->
	caller_id_record(Elements, #caller_id{}, nil);
parse_event('QueueMemberPaused', Elements) ->
	queue_member_record(Elements, #queue_member{}, nil);
parse_event('QueueMemberAdded', Elements) ->
	queue_member_record(Elements, #queue_member{}, nil);
parse_event('QueueMemberRemoved', Elements) ->
	queue_member_record(Elements, #queue_member{}, nil);
parse_event('ParkedCallTimeOut', Elements) ->
	parked_call_record(Elements, #park_call{}, nil);
parse_event('ParkedCallGiveUp', Elements) ->
	parked_call_record(Elements, #park_call{}, nil);
parse_event('AgentCalled', Elements) ->
	agent_call_record(Elements, #agent_call{}, nil);
parse_event('AgentDump', Elements) ->
	queue_event(Elements, #queue_event{}, nil);
parse_event('AgentComplete', Elements) ->
	queue_event(Elements, #queue_event{}, nil);
parse_event('AgentConnect', Elements) ->
	queue_event(Elements, #queue_event{}, nil);
parse_event('Agentlogin', Elements) ->
	agent_event_record(Elements, #agent_event{}, nil);
parse_event('Agentlogoff', Elements) ->
	agent_event_record(Elements, #agent_event{}, nil);
parse_event('Agentcallbacklogoff', Elements) ->
	agent_event_record(Elements, #agent_event{}, nil);
parse_event('Agentcallbacklogin', Elements) ->
	agent_event_record(Elements, #agent_event{}, nil);
parse_event('QueueMemberStatus', Elements) ->
	queue_member_record(Elements, #queue_member{}, nil);
parse_event('Dial', Elements) ->
	call_record(Elements, #call{}, nil);
parse_event('Reload', Elements) ->
	reload_record(Elements, #reload{}, nil);
parse_event('Shutdown', Elements) ->
	shutdown_record(Elements, #shutdown{}, nil);
parse_event('MessageWaiting', Elements) ->
	voicemail_record(Elements, #voicemail{}, nil);

%%% ============================================================================
%%%                      Events triggered by actions
%%% ============================================================================
parse_event('QueueEntry', Elements) ->
	queue_entry_record(Elements, #queue_entry{}, nil);
parse_event('QueueMember', Elements) ->
	queue_member_record(Elements, #queue_member{}, nil);
parse_event('QueueParams', Elements) ->
	queue_params_record(Elements, #queue_params{}, nil);
parse_event('PeerStatus', Elements) ->
	peer_status_record(Elements, #peer_status{}, nil);
parse_event('ParkedCall', Elements) ->
	parked_call_record(Elements, #park_call{}, nil);
parse_event('Agents', Elements) ->
	agent_record(Elements, #agent{}, nil);
parse_event('Status', Elements) ->
	channel_status_record(Elements, #channel_status{}, nil);
parse_event('ZapShowChannels', Elements) ->
	zap_channel_record(Elements, #zap_channel{}, nil);
parse_event('PeerEntry', Elements) ->
	sip_peer_record(Elements, #sip_peer{}, nil);
parse_event('DBGetResponse', Elements) ->
	db_response(Elements, {nil, nil});
parse_event('StatusComplete', Elements) ->
	event_complete(Elements);
parse_event('AgentsComplete', Elements) ->
	event_complete(Elements);
parse_event('PeerlistComplete', Elements) ->
	event_complete(Elements);
parse_event('QueueStatusComplete', Elements) ->
	event_complete(Elements);
parse_event('ParkedCallsComplete', Elements) ->
	event_complete(Elements);


%%% Event not handled. Normal if user events are used.
parse_event(Name, Elements) ->
	{Params, ActionID} = unknown_event(Elements, [], nil),
	error_logger:info_report([
		"Unknown event:",
		{name, Name},
		{elements, Params}
	]),
	{Params, ActionID}.

%%% ============================================================================
%%%                             Special cases
%%% ============================================================================
	
db_response([<<"Val: ", Value/binary>>|T], {_, ActionID}) ->
	db_response(T, {binary_to_list(Value), ActionID});
db_response([<<"ActionID: ", ActionID/binary>>|T], {Value, _}) ->
	db_response(T, {Value, binary_to_integer(ActionID)});
db_response([_|T], Acc) ->
	db_response(T, Acc);
db_response([], Acc) ->
	Acc.

unknown_event([<<"ActionID: ", ActionID/binary>>|T], Acc, _) ->
	unknown_event(T, Acc, binary_to_integer(ActionID));
unknown_event([Binary|T], Acc, ActionID) ->
	String = binary_to_list(Binary),
	{match, Start, Len} = regexp:first_match(String, "[A-Za-z]+: "),
	Name = list_to_atom(string:substr(String, Start, Len - 2)),
	Value = string:substr(String, Start + Len),
	unknown_event(T, [{Name, Value}|Acc], ActionID);
unknown_event([], Acc, ActionID) ->
	{Acc, ActionID}.

event_complete([<<"ActionID: ", ActionID/binary>>]) ->
	{nil, binary_to_integer(ActionID)};
event_complete([_|T]) -> % ignore information stuff
	event_complete(T).

%%% ============================================================================
%%%                  Build records from lists of binaries
%%% ============================================================================
%%% parse_iaxlist implemented 2009-11-20 by bartlomiej.puzon@erlang-consulting.com
parse_iaxlist([<<"Name", _Rest/binary>>|T]) ->
    Re = ?IAX_RE,
    {ok, MP} = re:compile(Re),
    parse_iaxlist2(T, #iax_peers{data = [], summary = undefined}, MP).

parse_iaxlist2([<<Line/binary>>], Data, _MP) ->
    Data#iax_peers{summary = binary_to_list(Line)};
parse_iaxlist2([<<Line/binary>>|T], #iax_peers{data = List} = Data, MP) ->
    case re:run(binary_to_list(Line), MP, [{capture, [1,2,3,4,6],list}]) of
	nomatch ->
	    List2 = [{iax_parse_error, Line} | List],
	    parse_iaxlist2(T, Data#iax_peers{data = List2}, MP);
	{match, [Name, Ip, NetMask, Port, Status]} ->
	    List2 = [#iax_peer{object_name = Name,
			       ip_address = Ip,
			       ip_mask = NetMask,
			       ip_port = list_to_integer(Port),
			       status = Status
			       } | List],
	    parse_iaxlist2(T, Data#iax_peers{data = List2}, MP)
    end.

originate_record([<<"ActionID: ", A/binary>>|T], Record, _) ->
	originate_record(T, Record, binary_to_integer(A));
originate_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	originate_record(T, Record#originate{privilege = privileges_list(Priv)},
		ActionID);
originate_record([<<"Channel: ", Chan/binary>>|T], Record, ActionID) ->
	originate_record(T, Record#originate{channel = binary_to_list(Chan)},
		ActionID);
originate_record([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	originate_record(T, Record#originate{context = binary_to_list(Context)},
		ActionID);
originate_record([<<"Exten: ", Exten/binary>>|T], Record, ActionID) ->
	originate_record(T, Record#originate{exten = binary_to_list(Exten)},
		ActionID);
originate_record([<<"Reason: ", Reason/binary>>|T], Record, ActionID) ->
	originate_record(T, Record#originate{reason = binary_to_integer(Reason)},
		ActionID);
originate_record([<<"Uniqueid: ", UID/binary>>|T], Record, ActionID) ->
	originate_record(T, Record#originate{unique_id = binary_to_list(UID)},
		ActionID);
originate_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in  record.", [Field])),
	originate_record(T, Record, ActionID);
originate_record([], Record, ActionID) ->
	{Record, ActionID}.

voicemail_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	voicemail_record(T, Record#voicemail{privilege = privileges_list(Priv)},
		ActionID);
voicemail_record([<<"Mailbox: ", Mailbox/binary>>|T], Record, ActionID) ->
	voicemail_record(T, Record#voicemail{mailbox = binary_to_list(Mailbox)},
		ActionID);
voicemail_record([<<"Waiting: ", Waiting/binary>>|T], Record, ActionID) ->
	voicemail_record(T, Record#voicemail{waiting = binary_to_list(Waiting)},
		ActionID);
voicemail_record([<<"New: ", New/binary>>|T], Record, ActionID) ->
	voicemail_record(T, Record#voicemail{new = binary_to_integer(New)},
		ActionID);
voicemail_record([<<"Old: ", Old/binary>>|T], Record, ActionID) ->
	voicemail_record(T, Record#voicemail{old = binary_to_integer(Old)},
		ActionID);
voicemail_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in voicemail record.", [Field])),
	voicemail_record(T, Record, ActionID);
voicemail_record([], Record, ActionID) ->
	{Record, ActionID}.

queue_event([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{privilege = privileges_list(Priv)},
		ActionID);
queue_event([<<"Queue: ", Queue/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{queue = binary_to_list(Queue)},
		ActionID);
queue_event([<<"Uniqueid: ", UID/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{unique_id = binary_to_list(UID)},
		ActionID);
queue_event([<<"Channel: ", Chan/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{channel = binary_to_list(Chan)},
		ActionID);
queue_event([<<"Member: ", Member/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{member = binary_to_list(Member)},
		ActionID);
queue_event([<<"HoldTime: ", HT/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{hold_time = binary_to_integer(HT)},
		ActionID);
queue_event([<<"TalkTime: ", TT/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{talk_time = binary_to_integer(TT)},
		ActionID);
queue_event([<<"Reason: ", Reason/binary>>|T], Record, ActionID) ->
	queue_event(T, Record#queue_event{reason = binary_to_atom(Reason)},
		ActionID);
queue_event([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in queue record.", [Field])),
	queue_event(T, Record, ActionID);
queue_event([], Record, ActionID) ->
	{Record, ActionID}.

agent_call_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{privilege = privileges_list(Priv)},
		ActionID);
agent_call_record([<<"AgentCalled: ", Agent/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{agent = binary_to_list(Agent)},
		ActionID);
agent_call_record([<<"CallerID: ", CID/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{caller_id = binary_to_list(CID)},
		ActionID);
agent_call_record([<<"CallerIDName: ", CIDName/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{
		caller_id_name = binary_to_list(CIDName)
	}, ActionID);
agent_call_record([<<"ChannelCalling: ", Chan/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{channel = binary_to_list(Chan)},
		ActionID);
agent_call_record([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{context = binary_to_list(Context)},
		ActionID);
agent_call_record([<<"Extension: ", Exten/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{extension = binary_to_list(Exten)},
		ActionID);
agent_call_record([<<"Priority: ", Priority/binary>>|T], Record, ActionID) ->
	agent_call_record(T, Record#agent_call{
		priority = binary_to_integer(Priority)
	},
	ActionID);
agent_call_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in agent_call record.", [Field])),
	agent_call_record(T, Record, ActionID);
agent_call_record([], Record, ActionID) ->
	{Record, ActionID}.

log_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	log_record(T, Record#log{privilege = privileges_list(Priv)}, ActionID);
log_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	log_record(T, Record#log{channel = binary_to_list(Channel)}, ActionID);
log_record([<<"Enabled: ", Enabled/binary>>|T], Record, ActionID) ->
	log_record(T, Record#log{enabled = eastrisk_types:to_bool(Enabled)},
		ActionID);
log_record([<<"Reason: ", Reason/binary>>|T], Record, ActionID) ->
	log_record(T, Record#log{reason = binary_to_list(Reason)}, ActionID);
log_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in log record.", [Field])),
	log_record(T, Record, ActionID);
log_record([], Record, ActionID) ->
	{Record, ActionID}.

registry_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	registry_record(T, Record#registry{privilege = privileges_list(Priv)},
		ActionID);
registry_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	registry_record(T, Record#registry{channel = binary_to_list(Channel)},
		ActionID);
registry_record([<<"Username: ", User/binary>>|T], Record, ActionID) ->
	registry_record(T, Record#registry{username = binary_to_list(User)},
		ActionID);
registry_record([<<"Domain: ", Domain/binary>>|T], Record, ActionID) ->
	registry_record(T, Record#registry{domain = binary_to_list(Domain)},
		ActionID);
registry_record([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	registry_record(T, Record#registry{status = binary_to_atom(Status)},
		ActionID);
registry_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in registry record.", [Field])),
	registry_record(T, Record, ActionID);
registry_record([], Record, ActionID) ->
	{Record, ActionID}.

hold_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	hold_record(T, Record#hold{privilege = privileges_list(Priv)},
		ActionID);
hold_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	hold_record(T, Record#hold{channel = binary_to_list(Channel)}, ActionID);
hold_record([<<"Uniqueid: ", UniqueID/binary>>|T], Record, ActionID) ->
	hold_record(T, Record#hold{unique_id = binary_to_list(UniqueID)}, ActionID);
hold_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in hold record.", [Field])),
	hold_record(T, Record, ActionID);
hold_record([], Record, ActionID) ->
	{Record, ActionID}.

dnd_state_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	dnd_state_record(T, Record#dnd_state{privilege = privileges_list(Priv)},
		ActionID);
dnd_state_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	dnd_state_record(T, Record#dnd_state{channel = binary_to_list(Channel)},
		ActionID);
dnd_state_record([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	dnd_state_record(T, Record#dnd_state{status = binary_to_atom(Status)},
		ActionID);
dnd_state_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in dnd_state record.", [Field])),
	dnd_state_record(T, Record, ActionID);
dnd_state_record([], Record, ActionID) ->
	{Record, ActionID}.

alarm_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	alarm_record(T, Record#alarm{privilege = privileges_list(Priv)},
		ActionID);
alarm_record([<<"Alarm: ", Alarm/binary>>|T], Record, ActionID) ->
	alarm_record(T, Record#alarm{alarm = binary_to_list(Alarm)}, ActionID);
alarm_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	alarm_record(T, Record#alarm{channel = binary_to_integer(Channel)},
		ActionID);
alarm_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in alarm record.", [Field])),
	alarm_record(T, Record, ActionID);
alarm_record([], Record, ActionID) ->
	{Record, ActionID}.

rename_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	rename_record(T, Record#rename{privilege = privileges_list(Priv)},
		ActionID);
rename_record([<<"Oldname: ", OldName/binary>>|T], Record, ActionID) ->
	rename_record(T, Record#rename{old_name = binary_to_list(OldName)},
		ActionID);
rename_record([<<"Newname", NewName/binary>>|T], Record, ActionID) ->
	rename_record(T, Record#rename{new_name = binary_to_list(NewName)},
		ActionID);
rename_record([<<"Uniqueid", UID/binary>>|T], Record, ActionID) ->
	rename_record(T, Record#rename{unique_id = binary_to_list(UID)},
		ActionID);
rename_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in rename record.", [Field])),
	rename_record(T, Record, ActionID);
rename_record([], Record, ActionID) ->
	{Record, ActionID}.

shutdown_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	shutdown_record(T, Record#shutdown{privilege = privileges_list(Priv)},
		ActionID);
shutdown_record([<<"Shutdown: ", Shutdown/binary>>|T], Record, ActionID) ->
	shutdown_record(T, Record#shutdown{shutdown = binary_to_atom(Shutdown)},
		ActionID);
shutdown_record([<<"Restart: ", Restart/binary>>|T], Record, ActionID) ->
	shutdown_record(T, Record#shutdown{
		restart = eastrisk_types:to_bool(Restart)
	}, ActionID);
shutdown_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in shutdown record.", [Field])),
	shutdown_record(T, Record, ActionID);
shutdown_record([], Record, ActionID) ->
	{Record, ActionID}.

agent_event_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	agent_event_record(T, Record#agent_event{privilege =
		privileges_list(Priv)}, ActionID);
agent_event_record([<<"Loginchan: ", LC/binary>>|T], Record, ActionID) ->
	agent_event_record(T, Record#agent_event{
		login_chan = binary_to_list(LC)}, ActionID);
agent_event_record([<<"Agent: ", Agent/binary>>|T], Record, ActionID) ->
	agent_event_record(T, Record#agent_event{
		agent_id = binary_to_integer(Agent)}, ActionID);
agent_event_record([<<"Logintime: ", LoginTime/binary>>|T], Record, ActionID) ->
	agent_event_record(T, Record#agent_event{
		login_time = binary_to_integer(LoginTime)}, ActionID);
agent_event_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	agent_event_record(T, Record, binary_to_integer(ActionID));
agent_event_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in agent_event record.", [Field])),
	agent_event_record(T, Record, ActionID);
agent_event_record([], Record, ActionID) ->
	{Record, ActionID}.

call_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{privilege = privileges_list(Priv)}, ActionID);
call_record([<<"Source: ", Source/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{source = binary_to_list(Source)}, ActionID);
call_record([<<"Destination: ", Dest/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{destination = binary_to_list(Dest)}, ActionID);
call_record([<<"CallerID: ", CID/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{caller_id = binary_to_list(CID)}, ActionID);
call_record([<<"CallerIDName: ", CIDName/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{caller_id_name = binary_to_list(CIDName)},
		ActionID);
call_record([<<"SrcUniqueID: ", SrcUID/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{src_unique_id = binary_to_list(SrcUID)},
		ActionID);
call_record([<<"DestUniqueID: ", DestUID/binary>>|T], Record, ActionID) ->
	call_record(T, Record#call{dest_unique_id = binary_to_list(DestUID)},
		ActionID);
call_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in call record.", [Field])),
	call_record(T, Record, ActionID);
call_record([], Record, ActionID) ->
	{Record, ActionID}.

ast_state_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	ast_state_record(T, Record#ast_state{privilege = privileges_list(Privilege)},
		ActionID);
ast_state_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	ast_state_record(T, Record#ast_state{channel = binary_to_list(Channel)}, 
		ActionID);
ast_state_record([<<"State: ", State/binary>>|T], Record, ActionID) ->
	ast_state_record(T, Record#ast_state{state = binary_to_atom(State)}, ActionID);
ast_state_record([<<"CallerID: ", CallerID/binary>>|T], Record, ActionID) ->
	ast_state_record(T, Record#ast_state{caller_id = binary_to_list(CallerID)},
		ActionID);
ast_state_record([<<"CallerIDName: ", CallerIDName/binary>>|T], Record, ActionID) ->
	ast_state_record(T, Record#ast_state{caller_id_name = 
		binary_to_list(CallerIDName)}, ActionID);
ast_state_record([<<"Uniqueid: ", Uniqueid/binary>>|T], Record, ActionID) ->
	ast_state_record(T, Record#ast_state{unique_id = binary_to_list(Uniqueid)},
		ActionID);
ast_state_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in ast_state record.", [Field])),
	ast_state_record(T, Record, ActionID);
ast_state_record([], Record, ActionID) ->
	{Record, ActionID}.

exten_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{privilege = privileges_list(Privilege)},
		ActionID);
exten_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{channel = binary_to_list(Channel)},
		ActionID);
exten_record([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{context = binary_to_list(Context)}, ActionID);
exten_record([<<"Extension: ", Extension/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{exten = binary_to_list(Extension)}, ActionID);
exten_record([<<"Priority: ", Priority/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{priority = binary_to_list(Priority)},
		ActionID);
exten_record([<<"Application: ", Application/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{application = binary_to_list(Application)},
		ActionID);
exten_record([<<"AppData: ", AppData/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{app_data = binary_to_list(AppData)},
		ActionID);
exten_record([<<"Uniqueid: ", Uniqueid/binary>>|T], Record, ActionID) ->
	exten_record(T, Record#exten{unique_id = binary_to_list(Uniqueid)},
		ActionID);
exten_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in exten record.", [Field])),
	exten_record(T, Record, ActionID);
exten_record([], Record, ActionID) ->
	{Record, ActionID}.

channel_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{privilege = privileges_list(Privilege)},
		ActionID);
channel_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{channel = binary_to_list(Channel)}, 
		ActionID);
channel_record([<<"State: ", State/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{state = binary_to_atom(State)}, ActionID);
channel_record([<<"CallerID: ", CallerID/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{caller_id = binary_to_list(CallerID)},
		ActionID);
channel_record([<<"CallerIDName: ", CallerIDName/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{caller_id_name = 
		binary_to_list(CallerIDName)}, ActionID);
channel_record([<<"CallerIDNum: ", CallerIDNum/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{caller_id_num = 
		binary_to_list(CallerIDNum)}, ActionID);
channel_record([<<"Uniqueid: ", Uniqueid/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{unique_id = binary_to_list(Uniqueid)},
		ActionID);
channel_record([<<"Event: ", Event/binary>>|T], Record, ActionID) ->
	channel_record(T, Record#channel{event = binary_to_list(Event)}, ActionID);
channel_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in channel record.", [Field])),
	channel_record(T, Record, ActionID);
channel_record([], Record, ActionID) ->
	{Record, ActionID}.

link_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{privilege = privileges_list(Privilege)}, ActionID);
link_record([<<"Channel1: ", Channel1/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{channel_1 = binary_to_list(Channel1)}, ActionID);
link_record([<<"Channel2: ", Channel2/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{channel_2 = binary_to_list(Channel2)}, ActionID);
link_record([<<"Uniqueid1: ", Uniqueid1/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{unique_id_1 = binary_to_list(Uniqueid1)}, ActionID);
link_record([<<"Uniqueid2: ", Uniqueid2/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{unique_id_2 = binary_to_list(Uniqueid2)}, ActionID);
link_record([<<"CallerID1: ", CallerID1/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{caller_id_1 = binary_to_list(CallerID1)}, ActionID);
link_record([<<"CallerID2: ", CallerID2/binary>>|T], Record, ActionID) ->
	link_record(T, Record#link{caller_id_2 = binary_to_list(CallerID2)}, ActionID);
link_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in link record.", [Field])),
	link_record(T, Record, ActionID);
link_record([], Record, ActionID) ->
	{Record, ActionID}.

hangup_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	hangup_record(T, Record#hangup{privilege = privileges_list(Privilege)}, ActionID);
hangup_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	hangup_record(T, Record#hangup{channel = binary_to_list(Channel)}, ActionID);
hangup_record([<<"Uniqueid: ", Uniqueid/binary>>|T], Record, ActionID) ->
	hangup_record(T, Record#hangup{unique_id = binary_to_list(Uniqueid)}, ActionID);
hangup_record([<<"Cause: ", Cause/binary>>|T], Record, ActionID) ->
	hangup_record(T, Record#hangup{cause = binary_to_integer(Cause)}, ActionID);
hangup_record([<<"Cause-txt: ", CauseTxt/binary>>|T], Record, ActionID) ->
	hangup_record(T, Record#hangup{cause_txt = binary_to_list(CauseTxt)}, ActionID);
hangup_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in hangup record.", [Field])),
	hangup_record(T, Record, ActionID);
hangup_record([], Record, ActionID) ->
	{Record, ActionID}.

caller_id_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	caller_id_record(T, Record#caller_id{privilege = 
		privileges_list(Privilege)}, ActionID);
caller_id_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	caller_id_record(T, Record#caller_id{channel = binary_to_list(Channel)}, ActionID);
caller_id_record([<<"CallerID: ", CallerID/binary>>|T], Record, ActionID) ->
	caller_id_record(T, Record#caller_id{caller_id = binary_to_list(CallerID)}, ActionID);
caller_id_record([<<"CallerIDName: ", CallerIDName/binary>>|T], Record, ActionID) ->
	caller_id_record(T, Record#caller_id{caller_id_name = 
		binary_to_list(CallerIDName)}, ActionID);
caller_id_record([<<"Uniqueid: ", UniqueID/binary>>|T], Record, ActionID) ->
	caller_id_record(T, Record#caller_id{unique_id = binary_to_list(UniqueID)}, ActionID);
caller_id_record([<<"CID-CallingPres: ", CIDCallingPres/binary>>|T], Record, ActionID) ->
	caller_id_record(T, Record#caller_id{cid_calling_pres = 
		cid_calling_pres(CIDCallingPres)}, ActionID);
caller_id_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in caller_id record.", [Field])),
	caller_id_record(T, Record, ActionID);
caller_id_record([], Record, ActionID) ->
	{Record, ActionID}.

reload_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	reload_record(T, Record#reload{privilege = privileges_list(Privilege)}, ActionID);
reload_record([<<"Message: ", Message/binary>>|T], Record, ActionID) ->
	reload_record(T, Record#reload{message = binary_to_list(Message)}, ActionID);
reload_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in reload record.", [Field])),
	reload_record(T, Record, ActionID);
reload_record([], Record, ActionID) ->
	{Record, ActionID}.

peer_status_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	peer_status_record(T, Record#peer_status{ privilege =
		privileges_list(Privilege)}, ActionID);
peer_status_record([<<"Peer: ", Peer/binary>>|T], Record, ActionID) ->
	peer_status_record(T, Record#peer_status{peer = binary_to_list(Peer)}, ActionID);
peer_status_record([<<"PeerStatus: ", PeerStatus/binary>>|T], Record, ActionID) ->
	peer_status_record(T, Record#peer_status{peer_status = 
		binary_to_atom(PeerStatus)}, ActionID);
peer_status_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in peer_status record.", [Field])),
	peer_status_record(T, Record, ActionID);
peer_status_record([], Record, ActionID) ->
	{Record, ActionID}.

agent_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	agent_record(T, Record, binary_to_integer(ActionID));
agent_record([<<"Agent: ", Id/binary>>|T], Record, ActionID) ->
	agent_record(T, Record#agent{id = binary_to_integer(Id)}, ActionID);
agent_record([<<"Name: ", Name/binary>>|T], Record, ActionID) ->
	agent_record(T, Record#agent{name = binary_to_list(Name)}, ActionID);
agent_record([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	agent_record(T, Record#agent{status = binary_to_atom(Status)}, ActionID);
agent_record([<<"LoggedInChan: ", Chan/binary>>|T], Record, ActionID) ->
	agent_record(T, Record#agent{channel = binary_to_list(Chan)}, ActionID);
agent_record([<<"LoggedInTime: ", Time/binary>>|T], Record, ActionID) ->
	agent_record(T, Record#agent{time = binary_to_integer(Time)}, ActionID);
agent_record([<<"TalkingTo: ", TalkingTo/binary>>|T], Record, ActionID) ->
	agent_record(T, Record#agent{talking_to = binary_to_list(TalkingTo)},
		ActionID);
agent_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in agent record.", [Field])),
	agent_record(T, Record, ActionID);
agent_record([], Record, ActionID) ->
	{Record, ActionID}.

exten_state_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	exten_state_record(T, Record, ActionID);
exten_state_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	exten_state_record(T, Record#exten_state{
		privilege = privileges_list(Privilege)}, ActionID);
exten_state_record([<<"Message: ", _Message/binary>>|T], Record, ActionID) ->
	exten_state_record(T, Record, ActionID);
exten_state_record([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	exten_state_record(T, Record#exten_state{status =
		binary_to_integer(Status)}, ActionID);
exten_state_record([<<"Exten: ", Exten/binary>>|T], Record, ActionID) ->
	exten_state_record(T, Record#exten_state{exten =
		binary_to_list(Exten)}, ActionID);
exten_state_record([<<"Hint: ", Hint/binary>>|T], Record, ActionID) ->
	exten_state_record(T, Record#exten_state{hint =
		binary_to_list(Hint)}, ActionID);
exten_state_record([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	exten_state_record(T, Record#exten_state{context = 
		binary_to_list(Context)}, ActionID);
exten_state_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in exten_state record.", [Field])),
	exten_state_record(T, Record, ActionID);
exten_state_record([], Record, ActionID) ->
	{Record, ActionID}.

mailbox_count_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	mailbox_count_record(T, Record, ActionID);
mailbox_count_record([<<"OldMessages: ", OldMsgs/binary>>|T], Record, ActionID) ->
	mailbox_count_record(T, Record#mbox_count{
		old_messages = binary_to_integer(OldMsgs)
	}, ActionID);
mailbox_count_record([<<"NewMessages: ", NewMsgs/binary>>|T], Record, ActionID) ->
	mailbox_count_record(T, Record#mbox_count{
		new_messages = binary_to_integer(NewMsgs)
	}, ActionID);
mailbox_count_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in mailbox_count record.", [Field])),
	mailbox_count_record(T, Record, ActionID);
mailbox_count_record([], Record, ActionID) ->
	{Record, ActionID}.

parked_call_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	parked_call_record(T, Record, ActionID);
parked_call_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{
		privilege = privileges_list(Privilege)}, ActionID);
parked_call_record([<<"Exten: ", Exten/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{exten = binary_to_list(Exten)}, ActionID);
parked_call_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{channel = binary_to_list(Channel)}, ActionID);
parked_call_record([<<"From: ", From/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{from = binary_to_list(From)}, ActionID);
parked_call_record([<<"Timeout: ", Timeout/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{timeout =
		binary_to_integer(Timeout)}, ActionID);
parked_call_record([<<"CallerID: ", CallerID/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{caller_id = 
		binary_to_list(CallerID)}, ActionID);
parked_call_record([<<"CallerIDName: ", CallerIDName/binary>>|T], Record, ActionID) ->
	parked_call_record(T, Record#park_call{caller_id_name = 
		binary_to_list(CallerIDName)}, ActionID);
parked_call_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in parked_call record.", [Field])),
	parked_call_record(T, Record, ActionID);
parked_call_record([], Record, ActionID) ->
	{Record, ActionID}.

queue_entry_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	queue_entry_record(T, Record, binary_to_integer(ActionID));
queue_entry_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{privilege = privileges_list(Priv)},
		ActionID);
queue_entry_record([<<"Queue: ", Queue/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{queue = binary_to_list(Queue)},
		ActionID);
queue_entry_record([<<"Position: ", Position/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{position =
		binary_to_integer(Position)}, ActionID);
queue_entry_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{channel =
		binary_to_list(Channel)}, ActionID);
queue_entry_record([<<"CallerID :", CallerID/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{caller_id = 
		binary_to_list(CallerID)}, ActionID);
queue_entry_record([<<"CallerIDName: ", CIDName/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{caller_id_name =
		binary_to_list(CIDName)}, ActionID);
queue_entry_record([<<"Wait: ", Wait/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{wait = binary_to_integer(Wait)},
		ActionID);
queue_entry_record([<<"Count: ", Count/binary>>|T], Record, ActionID) ->
	queue_entry_record(T, Record#queue_entry{count = binary_to_integer(Count)},
		ActionID);
queue_entry_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in queue_entry record.", [Field])),
	queue_entry_record(T, Record, ActionID);
queue_entry_record([], Record, ActionID) ->
	{Record, ActionID}.

queue_member_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	queue_member_record(T, Record, binary_to_integer(ActionID));
queue_member_record([<<"Privilege: ", Priv/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{
		privilege = privileges_list(Priv)}, ActionID);
queue_member_record([<<"Queue: ", Queue/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{queue = 
		binary_to_list(Queue)}, ActionID);
queue_member_record([<<"Location: ", Location/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{location = 
		binary_to_list(Location)}, ActionID);
queue_member_record([<<"Membership: ", Membership/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{membership = 
		binary_to_list(Membership)}, ActionID);
queue_member_record([<<"Penalty: ", Penalty/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{penalty =
		binary_to_integer(Penalty)}, ActionID);
queue_member_record([<<"CallsTaken: ", CallsTaken/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{calls_taken = 
		binary_to_integer(CallsTaken)}, ActionID);
queue_member_record([<<"LastCall: ", LastCall/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{last_call =
		binary_to_integer(LastCall)}, ActionID);
queue_member_record([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{status = 
		binary_to_integer(Status)}, ActionID);
queue_member_record([<<"Paused: ", Paused/binary>>|T], Record, ActionID) ->
	queue_member_record(T, Record#queue_member{
		paused = eastrisk_types:to_bool(Paused)
	}, ActionID);
queue_member_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in queue_member record.", [Field])),
	queue_member_record(T, Record, ActionID);
queue_member_record([], Record, ActionID) ->
	{Record, ActionID}.

queue_params_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	queue_params_record(T, Record, binary_to_integer(ActionID));
queue_params_record([<<"Queue: ", Queue/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{queue = binary_to_list(Queue)}, ActionID);
queue_params_record([<<"Max: ", Max/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{max = binary_to_integer(Max)}, ActionID);
queue_params_record([<<"Calls: ", Calls/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{calls =
		binary_to_integer(Calls)}, ActionID);
queue_params_record([<<"Holdtime: ", Holdtime/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{holdtime =
		binary_to_integer(Holdtime)}, ActionID);
queue_params_record([<<"Completed: ", Completed/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{completed = 
		binary_to_integer(Completed)}, ActionID);
queue_params_record([<<"Abandoned: ", Abandoned/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{abandoned =
		binary_to_integer(Abandoned)}, ActionID);
queue_params_record([<<"ServiceLevel: ", ServiceLevel/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{service_level =
		binary_to_integer(ServiceLevel)}, ActionID);
queue_params_record([<<"ServicelevelPerf: ", ServicelevelPerf/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{service_level_perf = 
		binary_to_float(ServicelevelPerf)}, ActionID);
queue_params_record([<<"Weight: ", Weight/binary>>|T], Record, ActionID) ->
	queue_params_record(T, Record#queue_params{weight =
		binary_to_integer(Weight)}, ActionID);
queue_params_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in queue_params record.", [Field])),
	queue_params_record(T, Record, ActionID);
queue_params_record([], Record, ActionID) ->
	{Record, ActionID}.

sip_peer_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	sip_peer_record(T, Record, binary_to_integer(ActionID));
sip_peer_record([<<"Channeltype: ", Channeltype/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{channeltype = 
		binary_to_list(Channeltype)}, ActionID);
sip_peer_record([<<"ObjectName: ", ObjectName/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{object_name = 
		binary_to_list(ObjectName)}, ActionID);
sip_peer_record([<<"ChanObjectType: ", COT/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{chan_object_type =
		binary_to_atom(COT)}, ActionID);
sip_peer_record([<<"IPaddress: ", IPaddress/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{ip_address = binary_to_list(IPaddress)},
		ActionID);
sip_peer_record([<<"IPport: ", IPport/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{ip_port = binary_to_integer(IPport)},
		ActionID);
sip_peer_record([<<"Dynamic: ", Dynamic/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{
		dynamic = eastrisk_types:to_bool(Dynamic)
	}, ActionID);
sip_peer_record([<<"Natsupport: ", Natsupport/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{nat_support =
		eastrisk_types:to_bool(Natsupport)}, ActionID);
sip_peer_record([<<"ACL: ", ACL/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{acl = eastrisk_types:to_bool(ACL)}, ActionID);
sip_peer_record([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	sip_peer_record(T, Record#sip_peer{status = binary_to_list(Status)}, ActionID);
sip_peer_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in sip_peer record.", [Field])),
	sip_peer_record(T, Record, ActionID);
sip_peer_record([], Record, ActionID) ->
	{Record, ActionID}.

sip_peer_details([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	sip_peer_details(T, Record, binary_to_atom(ActionID));
sip_peer_details([<<"Channeltype: ", Channeltype/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{channeltype = 
		binary_to_list(Channeltype)}, ActionID);
sip_peer_details([<<"ObjectName: ", ObjectName/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{object_name = 
		binary_to_list(ObjectName)}, ActionID);
sip_peer_details([<<"ChanObjectType: ", COT/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{chan_object_type =
		binary_to_list(COT)}, ActionID);
sip_peer_details([<<"SecretExist: ", SecretExist/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{secret_exists =
		eastrisk_types:to_bool(SecretExist)}, ActionID);
sip_peer_details([<<"MD5SecretExist: ", MD5Secret/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{md5_secret_exists =
		eastrisk_types:to_bool(MD5Secret)
	}, ActionID);
sip_peer_details([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{context = 
		binary_to_list(Context)}, ActionID);
sip_peer_details([<<"Language: ", Language/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{language =
		binary_to_list(Language)}, ActionID);
sip_peer_details([<<"AMAflags: ", AMAflags/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{ama_flags = 
		binary_to_list(AMAflags)
	}, ActionID);
sip_peer_details([<<"CID-CallingPres: ", CIDCallingPres/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{cid_calling_pres =
		cid_calling_pres(CIDCallingPres)}, ActionID);
sip_peer_details([<<"Callgroup: ", Callgroup/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{callgroup =
		binary_to_list(Callgroup)}, ActionID);
sip_peer_details([<<"Pickupgroup: ", Pickupgroup/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{pickupgroup = 
		binary_to_list(Pickupgroup)}, ActionID);
sip_peer_details([<<"VoiceMailbox: ", VoiceMailbox/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{voice_mailbox =
		binary_to_list(VoiceMailbox)}, ActionID);
sip_peer_details([<<"LastMsgsSent: ", LastMsgsSent/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{last_msg_sent =
		binary_to_integer(LastMsgsSent)}, ActionID);
sip_peer_details([<<"Call limit: ", CallLimit/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{call_limit =
		binary_to_integer(CallLimit)}, ActionID);
sip_peer_details([<<"Dynamic: ", Dynamic/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{dynamic =
		eastrisk_types:to_bool(Dynamic)}, ActionID);
sip_peer_details([<<"Callerid: ", Callerid/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{caller_id = binary_to_list(Callerid)}, ActionID);
sip_peer_details([<<"RegExpire: ", RegExpire/binary>>|T], Record, ActionID) ->
	[Seconds, _S] = string:tokens(binary_to_list(RegExpire), " "),
	sip_peer_details(T, Record#sip_peer_details{reg_expire =
		list_to_integer(Seconds)}, ActionID);
sip_peer_details([<<"SIP-AuthInsecure: ", SIPAuthInsecure/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_auth_insecure =
		binary_to_atom(SIPAuthInsecure)}, ActionID);
sip_peer_details([<<"SIP-NatSupport: ", SIPNatSupport/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_nat_support =
		binary_to_atom(SIPNatSupport)}, ActionID);
sip_peer_details([<<"ACL: ", ACL/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{acl = eastrisk_types:to_bool(ACL)}, ActionID);
sip_peer_details([<<"SIP-CanReinvite: ", SIPCanReinvite/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_can_reinvite =
		eastrisk_types:to_bool(SIPCanReinvite)}, ActionID);
sip_peer_details([<<"SIP-PromiscRedir: ", SIPPromiscRedir/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_primisc_redir =
		eastrisk_types:to_bool(SIPPromiscRedir)}, ActionID);
sip_peer_details([<<"SIP-UserPhone: ", SIPUserPhone/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_user_phone = 
		eastrisk_types:to_bool(SIPUserPhone)}, ActionID);
sip_peer_details([<<"SIP-DTMFmode: ", SIPDTMFmode/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_dtmf_mode =
		binary_to_atom(SIPDTMFmode)}, ActionID);
sip_peer_details([<<"SIPLastMsg: ", SIPLastMsg/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_last_msg =
		binary_to_integer(SIPLastMsg)}, ActionID);
sip_peer_details([<<"ToHost: ", ToHost/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{to_host = 
		binary_to_list(ToHost)}, ActionID);
sip_peer_details([<<"Address-IP: ", AddressIP/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{ip_address = 
		binary_to_list(AddressIP)}, ActionID);
sip_peer_details([<<"Address-Port: ", AddressPort/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{ip_port =
		binary_to_integer(AddressPort)}, ActionID);
sip_peer_details([<<"Default-addr-IP: ", DefaultAddrIP/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{default_ip_address =
		binary_to_list(DefaultAddrIP)}, ActionID);
sip_peer_details([<<"Default-addr-port: ", DefaultAddrPort/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{default_ip_port =
		binary_to_integer(DefaultAddrPort)}, ActionID);
sip_peer_details([<<"Default-Username: ", DefaultUsername/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{default_user_name = 
		binary_to_list(DefaultUsername)}, ActionID);
sip_peer_details([<<"Codecs: ", Codecs/binary>>|T], Record, ActionID) ->
	[[$0, $x|ID], Names] = string:tokens(binary_to_list(Codecs), " "),
	sip_peer_details(T, Record#sip_peer_details{codecs =
		{erlang:list_to_integer(ID, 16),
		 list_to_atoms(string:tokens(Names, "|()"), [])}}, ActionID);
sip_peer_details([<<"CodecOrder: ", CodecOrder/binary>>|T], Record, ActionID) ->
	Strings = string:tokens(binary_to_list(CodecOrder), ","),
	CodecList = list_to_atoms(Strings, []),
	sip_peer_details(T, Record#sip_peer_details{codec_order = CodecList}, ActionID);
sip_peer_details([<<"Status: ", Status/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{status = 
		binary_to_list(Status)}, ActionID);
sip_peer_details([<<"SIP-Useragent: ", SIPUseragent/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{sip_user_agent = 
		binary_to_list(SIPUseragent)}, ActionID);
sip_peer_details([<<"Reg-Contact : ", RegContact/binary>>|T], Record, ActionID) ->
	sip_peer_details(T, Record#sip_peer_details{reg_contact = 
		binary_to_list(RegContact)}, ActionID);
sip_peer_details([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in sip_peer_details record.", [Field])),
	sip_peer_details(T, Record, ActionID);
sip_peer_details([], Record, ActionID) ->
	{Record, ActionID}.

channel_status_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	channel_status_record(T, Record, binary_to_integer(ActionID));
channel_status_record([<<"Privilege: ", Privilege/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{privilege = 
		privileges_list(Privilege)}, ActionID);
channel_status_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{channel =
		binary_to_list(Channel)}, ActionID);
channel_status_record([<<"CallerID: ", CallerID/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{caller_id =
		binary_to_list(CallerID)}, ActionID);
channel_status_record([<<"CallerIDName: ", CallerIDName/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{caller_id_name =
		binary_to_list(CallerIDName)}, ActionID);
channel_status_record([<<"Account: ", Account/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{account = 
		binary_to_list(Account)}, ActionID);
channel_status_record([<<"State: ", State/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{state =
		binary_to_atom(State)}, ActionID);
channel_status_record([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{context =
		binary_to_list(Context)}, ActionID);
channel_status_record([<<"Extension: ", Extension/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{extension =
		binary_to_list(Extension)}, ActionID);
channel_status_record([<<"Priority: ", Priority/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{priority =
		binary_to_list(Priority)}, ActionID);
channel_status_record([<<"Seconds: ", Seconds/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{seconds =
		binary_to_integer(Seconds)}, ActionID);
channel_status_record([<<"Link: ", Link/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{link =
		binary_to_list(Link)}, ActionID);
channel_status_record([<<"Uniqueid: ", Uniqueid/binary>>|T], Record, ActionID) ->
	channel_status_record(T, Record#channel_status{unique_id =
		binary_to_list(Uniqueid)}, ActionID);
channel_status_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in channel_status record.", [Field])),
	channel_status_record(T, Record, ActionID);
channel_status_record([], Record, ActionID) ->
	{Record, ActionID}.

zap_channel_record([<<"ActionID: ", ActionID/binary>>|T], Record, _) ->
	zap_channel_record(T, Record, binary_to_integer(ActionID));
zap_channel_record([<<"Channel: ", Channel/binary>>|T], Record, ActionID) ->
	zap_channel_record(T, Record#zap_channel{channel =
		binary_to_list(Channel)}, ActionID);
zap_channel_record([<<"Signalling: ", Signalling/binary>>|T], Record, ActionID) ->
	zap_channel_record(T, Record#zap_channel{signalling = 
		binary_to_list(Signalling)}, ActionID);
zap_channel_record([<<"Context: ", Context/binary>>|T], Record, ActionID) ->
	zap_channel_record(T, Record#zap_channel{context = 
		binary_to_list(Context)}, ActionID);
zap_channel_record([<<"DND: ", DND/binary>>|T], Record, ActionID) ->
	zap_channel_record(T, Record#zap_channel{dnd = eastrisk_types:to_bool(DND)}, ActionID);
zap_channel_record([<<"Alarm: ", Alarm/binary>>|T], Record, ActionID) ->
	zap_channel_record(T, Record#zap_channel{alarm = 
		binary_to_list(Alarm)}, ActionID);
zap_channel_record([Field|T], Record, ActionID) ->
	?warning(io_lib:format("Ignoring ~p in zap_channel record.", [Field])),
	zap_channel_record(T, Record, ActionID);
zap_channel_record([], Record, ActionID) ->
	{Record, ActionID}.

%%% ============================================================================
%%%                 Helper functions, type conversions etc.
%%% ============================================================================

binary_to_atom(Binary) ->
	list_to_atom(binary_to_list(Binary)).

binary_to_integer(Binary) ->
	list_to_integer(binary_to_list(Binary)).

binary_to_float(Binary) ->
	list_to_float(binary_to_list(Binary)).

privileges_list(Binary) ->
	list_to_atoms(string:tokens(binary_to_list(Binary), ","), []).

cid_calling_pres(Binary) ->
	String = binary_to_list(Binary),
	{match, Start, Len} = regexp:first_match(String, "(-)?[0-9]+"),
	string:substr(String, Start, Len).

list_to_atoms([String|T], Acc) ->
	list_to_atoms(T, [convert_atom(list_to_atom(String))|Acc]);
list_to_atoms([], Acc) ->
	Acc.

convert_atom('Call') -> call;
convert_atom(Any)    -> Any.
