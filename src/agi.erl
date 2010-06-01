%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.3, 2006-08-10
%%% @copyright 2006 Erlang Training and Consulting
%%%
%%% @doc
%%% AGI Library functions.
%%%
%%% <p>
%%% This module provides an Erlang interface to AGI functions. A connection must
%%% have been established between Asterisk and the AGI Server (see {@link
%%% agi_server}) before this module can be used.
%%% </p>
%%% <p>
%%% Every function takes a ChannelPid as the first argument. This pid will be
%%% distributed by the {@link agi_events} when a channel is created.
%%% </p>
%%% @end
%%% ----------------------------------------------------------------------------
-module(agi).

%%% AGI Functions (also API really)
-export([answer/1,
         channel_status/1,
         channel_status/2,
		 database_del/3,
		 database_deltree/2,
		 database_deltree/3,
		 database_get/3,
		 database_put/4,
		 exec/3,
		 get_data/2,
		 get_data/3,
		 get_data/4,
		 get_full_variable/2,
		 get_full_variable/3,
		 get_option/3,
		 get_option/4,
		 get_variable/2,
		 hangup/1,
		 hangup/2,
         noop/2,	
		 receive_char/2,
		 record_file/8,
		 say_alpha/3,
		 say_date/3,
         say_datetime/3,
         say_datetime/4,
         say_datetime/5,
		 say_number/3,
		 say_digits/3,
		 say_phonetic/3,
		 say_time/3,
		 send_image/2,
		 send_text/2,
		 set_auto_hangup/2,
		 set_callerid/2,
		 set_context/2,
		 set_extension/2,
		 set_music_on/2,
		 set_music_on/3,
		 set_priority/2,
		 set_variable/3,
		 stream_file/3,
		 stream_file/4,
		 tdd_mode/2,
		 wait_for_digit/2,
         verbose/3]).

%%% Intermodule exports
-export([parse_init_arg/2]).

-include("agi.hrl").

%%% AGI Commands
%%% All AGI commands takes a pid() as the first argument.
%%% That port must be the one which was returned by init/0.

%% -----------------------------------------------------------------------------
%% @spec answer(ChannelPid::pid()) -> {ok, Value}
%%                                    Value = integer()
%% @doc
%% Answers the channel (if not already in an answered state).
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Failure</li>
%% <li> 0, Success</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
answer(ChannelPid) ->
	integer_or_error(execute(ChannelPid, "ANSWER\n")).

%% -----------------------------------------------------------------------------
%% @spec channel_status(ChannelPid::pid()) -> {ok, Value} | agi_error()
%% @doc
%% Queries the status of the current channel.
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Channel is down and available</li>
%% <li>1,  Channel is down, but reserved</li>
%% <li>2,  Channel is off-hook</li>
%% <li>3,  Digits have been dialled</li>
%% <li>4,  Line is ringing</li>
%% <li>5,  Line is up</li>
%% <li>6,  Line is busy</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
channel_status(ChannelPid) ->
	integer_or_error(execute(ChannelPid, "CHANNEL STATUS\n")).

%% -----------------------------------------------------------------------------
%% @spec channel_status(ChannelPid::pid(), ChannelName::string()) ->
%%                                                  {ok, Value}
%% @doc
%% Queries the status of channel <em>ChannelName</em>. 
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Channel is down and available</li>
%% <li>1,  Channel is down, but reserved</li>
%% <li>2,  Channel is off-hook</li>
%% <li>3,  Digits have been dialled</li>
%% <li>4,  Line is ringing</li>
%% <li>5,  Line is up</li>
%% <li>6,  Line is busy</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
channel_status(ChannelPid, ChannelName) ->
	Command = io_lib:format("CHANNEL STATUS ~s\n", [ChannelName]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec database_del(ChannelPid::pid(), Family::string(), Key::string()) ->
%%                                              {ok, Value} | agi_error()
%%                                              Value = integer()
%% @doc
%% Deletes an entry from the Asterisk database for the specified family and key.
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Failure (value not set)</li>
%% <li>1,  Success (value deleted)</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
database_del(ChannelPid, Family, Key) ->
	Command = io_lib:format("DATABASE DEL ~s ~s\n", [Family, Key]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec database_deltree(ChannelPid::pid(), Family::sting()) -> {ok, Value}
%%                                                        Value = integer()
%% @doc
%% Deletes a family and from the Asterisk database.
%% <p>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Failure</li>
%% <li>1,  Success</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
database_deltree(ChannelPid, Family) ->
	Command = io_lib:format("DATABASE DELTREE ~s\n", [Family]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec database_deltree(ChannelPid::pid(), Family::sting(), KeyTree::string()) -> 
%%                                              {ok, Value} | agi_error()
%% @doc
%% Deletes a family and/or keytree from the Asterisk database.
%% <p>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Failure</li>
%% <li>1,  Success</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
database_deltree(ChannelPid, Family, KeyTree) ->
	Command = io_lib:format("DATABASE DELTREE ~s ~s\n", [Family, KeyTree]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec database_get(ChannelPid::pid(), Family::string(), Key::string()) ->
%%                    {ok, 0} | {ok, {1, Value}} | agi_error()
%%                    Status = integer()
%%                    Value = string()
%% @doc
%% Retrieves a value from the Asterisk database for the specified family and
%% key.
%% <p>
%% Return values:
%% <ul>
%% <li>0</li>
%% <li>{1, <em>Value</em>}, Value is retrieved from database</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
database_get(ChannelPid, Family, Key) ->
	Command = io_lib:format("DATABASE GET ~s ~s\n", [Family, Key]),
	case execute(ChannelPid, Command) of
		{ok, {Status, Value}} ->
			{ok, {list_to_integer(Status), Value}};
		{ok, Status} ->
			{ok, list_to_integer(Status)};
		Else ->
			Else
	end.

%% -----------------------------------------------------------------------------
%% @spec database_put(ChannelPid::pid(), Family::string(), Key::string(),
%%                    Value::string()) -> {ok, Status} | agi_error()
%%                    Status = integer()
%% @doc
%% Adds or updates an entry in the Asterisk database for the specified family
%% and key, with the specified value.
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Failure</li>
%% <li>1,  Success</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
database_put(ChannelPid, Family, Key, Value) ->
	Command = io_lib:format("DATABASE PUT ~s ~s ~s\n", [Family, Key, Value]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec exec(ChannelPid::pid(), Application::string(), Options::string()) -> 
%%                                           {ok, Value} | agi_error()
%%                                           Value = integer()
%% @doc
%% Executes the specified dialplan application, including options.
%% <p>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0,  Success</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
exec(ChannelPid, Application, Options) ->
	Command = io_lib:format("EXEC ~s ~s\n", [Application, Options]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_data(ChannelPid::pid(), Filename::string()) -> {ok, Value} |
%%                                                     {ok, {Value, timeout}}
%% @doc
%% Plays an audio file and accepts DTMF digits.
%% <p>
%% Plays the audio file specified by <em>Filename</em> and accepts DTMF
%% digits.<br />
%% Similar to the dialplan application Background().
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li><em>Value</em>, Digits received from the caller</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_data(ChannelPid, Filename) ->
	Command = io_lib:format("GET DATA ~s\n", [Filename]),
	int_and_atom_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_data(ChannelPid::pid(), Filename::string(),
%%                Timeout::integer()) -> {ok, Value} | {ok, {Value, timeout}}
%% @doc
%% Plays a file and accepts DTMF digits, with a timeout.
%% <p>
%% Works as {@link get_data/2}, but has a timeout in milliseconds.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li><em>Value</em>, Digits received from the caller</li>
%% <li>{<em>Value</em>, timeout}, If a timeout occurred, digits received from
%% the called, before the timeout</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_data(ChannelPid, Filename, Timeout) ->
	Command = io_lib:format("GET DATA ~s ~B \n", [Filename, Timeout]),
	int_and_atom_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_data(ChannelPid::pid(), Filename::string(), Timeout::integer(),
%%                MaxDigits::integer()) -> {ok, Value} | {ok, {Value, timeout}}
%%                                                  List = [Arg]
%%                                                  Arg = string() | integer()
%% @doc
%% Plays a file and accepts DTMF digits, with a timeout and a maximum amount of
%% digits.
%% <p>
%% Works as {@link get_data/3} but will stop when <em>MaxDigits</em> DTMF
%% digits has been entered.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li><em>Value</em>, Digits received from the caller, where the number of
%% digits is <em>MaxDigits</em></li>
%% <li>{<em>Value</em>, timeout}, If a timeout occurred, digits received from
%% the called, before the timeout</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_data(ChannelPid, Filename, Timeout, MaxDigits) ->
	Command = io_lib:format("GET DATA ~s ~B ~B\n", [Filename, Timeout, MaxDigits]),
	int_and_atom_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_full_variable(ChannelPid::pid(), VariableName::string()) -> 
%%                                         {ok, 0} | {ok, {1, Value}}
%%                                         Value = string()
%% @doc
%% Returns the variable specified by <em>VariableName</em> if it is set.
%% 
%%
%% <p>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0,  No channel or variable not set</li>
%% <li><em>Value</em>, Value as string()</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_full_variable(ChannelPid, VariableName) ->
	Command = io_lib:format("GET FULL VARIABLE ~s\n", [VariableName]),
	int_and_string_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_full_variable(ChannelPid::pid(), VariableName::string(),
%%                         ChannelName::string()) -> {ok, 0} | {ok, {1, Value}}
%%                                                   Value = string()
%% @doc
%% Behaves like {@link get_full_variable/2} but specifies the
%% <em>ChannelName</em>.
%% <p>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0,  No channel or variable not set</li>
%% <li><em>Value</em>, Value as string()</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_full_variable(ChannelPid, VariableName, ChannelName) ->
	Command = io_lib:format("GET FULL VARIABLE ~s ~s\n",
	                        [VariableName, ChannelName]),
	int_and_string_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_option(ChannelPid::pid(), Filename::string(),
%%                  EscapeDigits::string()) ->    {ok, {Value, EndPoint}}
%%                                                 Value = integer()
%%                                                 EndPoint = integer()
%% @doc
%% Behaves the same as {@link stream_file/3}.
%% <p>
%% Return values:
%% <ul>
%% <li><em>Value</em>, ASCII value of the digits received, in decimal.</li>
%% </ul>
%% <em>EndPoint</em> refers to how long the file has played for.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_option(ChannelPid, FileName, EscapeDigits) ->
	Command = io_lib:format("GET OPTION ~s ~s\n",
	                        [FileName, escape_digits(EscapeDigits)]),
	int_and_endpoint_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec get_option(ChannelPid::pid(), Filename::string(), 
%%                  EscapeDigits::string(), Timeout::integer()) ->
%%                                                     {ok, {Value, EndPoint}}
%%                                                      Value = integer()
%%                                                      EndPoint = integer()
%% @doc
%% Behaves the same as {@link get_option/3}, but has a timeout (in seconds).
%% <p>
%% Return values:
%% <ul>
%% <li>0, timeout</li>
%% <li><em>Value</em>, ASCII value of the digits received, in decimal.</li>
%% </ul>
%% <em>EndPoint</em> refers to how long the file has played for.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
get_option(ChannelPid, FileName, EscapeDigits, Timeout) ->
	Command = io_lib:format("GET OPTION ~s ~s ~B\n",
	                       [FileName, escape_digits(EscapeDigits), Timeout]),
	int_and_endpoint_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec get_variable(ChannelPid::pid(), VariableName::string()) ->
%%                                              {ok, 0} | {ok, {1, Value}}
%%                                              Value = string()
%% @doc
%% Returns the value of a variable.
%% <p>
%% This command does not understand complex variables or built-in variables; use
%% the {@link get_full_variable/2} or {@link get_full_variable/3} command if
%% your application requires these types of variables.
%% Return values:
%% </p>
%% <p>
%% <ul>
%% <li>0, No channel, or variable not set</li>
%% <li>{1, <em>Value</em>}, Variable set and returned</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
get_variable(ChannelPid, VariableName) ->
	Command = io_lib:format("GET VARIABLE ~s\n", [VariableName]),
	int_and_string_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec hangup(ChannelPid::pid()) -> {ok, Value} | agi_error()
%%                                   Value = integer()
%% @doc
%% Hangs up the current channel.
%% <p>
%% Usually the socket between the AGI -> Erlang bridge seem to close before the
%% return value has been sent.
%% This causes this function to return {error, {-1, Message}} most of the time.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>1, Hangup successful</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
hangup(ChannelPid) ->
	integer_or_error(execute(ChannelPid, "HANGUP\n")).

%% ----------------------------------------------------------------------------
%% @spec hangup(ChannelPid::pid(), ChannelName::string()) -> {ok, Value}
%%                                                           Value = integer()
%% @doc
%% Hangs up the specified channel.
%% <p>
%% Usually the socket between the AGI -> Erlang bridge seem to close before the
%% return value has been sent.
%% This causes this function to return {error, {-1, Message}} most of the time.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, The specified channel does not exist</li>
%% <li> 1, Hangup successful</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
hangup(ChannelPid, ChannelName) ->
	Command = io_lib:format("HANGUP ~s\n", [ChannelName]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec noop(ChannelPid::pid(), Text::string()) -> {ok, Value}
%%                                                  Value = integer()
%% @doc
%% Performs <em>no</em> operation, but will cause the text passed to it to
%% appear in the Asterisk console.
%% <p>
%% Usually used for debugging purposes.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
noop(ChannelPid, Text) ->
	Command = io_lib:format("NoOp ~s\n", [escape_digits(Text)]),
	integer_or_error(execute(ChannelPid, Command)).


%% ----------------------------------------------------------------------------
%% @spec receive_char(ChannelPid::pid(), Timeout::integer()) -> 
%%                                                  {ok, {Result, Reason}}
%%                                                  Result = string()
%%                                                  Reason = string()
%% @doc
%% Receives a character of text on a channel.
%% <p>
%% The <em>Timeout</em> specifies how the maximum amount of seconds to wait for
%% a character, where 0 means infinity. Note that most channels does not support
%% the reception of text.
%% </p>
%% <p>
%% NOTE: Not sure about these return values.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>{-1,   "hangup"}, Failure or hangup</li>
%% <li>{Char, "timeout"}, Failure or hangup</li>
%% <li><em>Value</em>, ASCII value of character, in decimal</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
receive_char(ChannelPid, Timeout) ->
	Command = io_lib:format("RECEIVE CHAR ~B\n", [Timeout]),
	execute(ChannelPid, Command).

%% ----------------------------------------------------------------------------
%% @spec record_file(ChannelPid::pid(), Filename::string(), Format::string(),
%%                   EscapeDigits::string(), Timeout::integer(),
%%                   OffsetSamples::integer(), Beep::bool(),
%%                   Silence::integer()) -> {ok, {Result, Reason, Endpoint}}
%%                                            Result = integer()
%%                                            Reason = atom()
%%                                            Endpoint = integer()
%% @doc
%% Records channel audio to a specified file.
%% <p>
%% Records the audio to <em>Filename</em> in <em>Format</em> until any of
%% <em>EscapeDigits</em> are received or <em>Timeout</em> milliseconds has
%% passed.
%% <p>
%% If <em>Timeout</em> is set to -1, no timeout will happen.
%% <p>
%% </p>
%% If <em>Beep</em> is true, a beep will be played before the recording is
%% started.
%% </p>
%% <p>
%% If <em>OffsetSamples</em> != 0, it will seek to the offset without exceeding
%% the end of the file. 
%% </p>
%% <em>Silence</em> is the number of seconds of silence
%% allowed before the function returns despite the lack of DTMF digits or
%% reaching the timeout, set it to 0 to disable. 
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>{0, hangup, EndPoint}, The channel was hung up, this will probably never
%%     happend, instead -1 error code will be received.</li>
%% <li>{Value, dtmf, EndPoint}, where <em>Value</em> is a list of ASCII values
%% representing the DTMF characters sent.</li>
%% <li>{0, timeout, Endpoint}, whenever a timeout has stopped the recording,
%% either due to <em>Silence</em> or due to <em>Timout</em>.</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
record_file(ChannelPid, Filename, Format, EscapeDigits, Timeout,
            OffsetSamples, Beep, Silence) ->
	SilenceArg = case Silence of
		0 -> "";
		_ -> io_lib:format("s=~B", [Silence])
	end,
	BeepArg = case Beep of
		false -> "";
		true  -> "BEEP"
	end,
	OffsetArg = case OffsetSamples of
		0 -> "";
		_ -> io_lib:format("~B", [OffsetSamples])
	end,
	Command = io_lib:format("RECORD FILE ~s ~s ~s ~B ~s ~s ~s\n",
		[Filename, Format, escape_digits(EscapeDigits), Timeout, 
		 OffsetArg, BeepArg, SilenceArg]),
	int_tuple_int_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec say_alpha(ChannelPid::pid(), Number::string(),
%%                 EscapeDigits::string()) -> {ok, Value}
%% @doc
%% Plays a character string.
%% <p>
%% Plays the character string <em>Number</em>, returning the first of the
%% <em>EscapeDigits</em> pressed.
%% </p>
%% <p>
%% Note! Does <em>not</em> return early if any escape digits are pressed even
%% though it says so in the documentation.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0, Playback completed without an escape digit peing pressed</li>
%% <li><em>Value</em>, ASCII value of the digit pressed</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
say_alpha(ChannelPid, Number, EscapeDigits) ->
	Command = io_lib:format("SAY ALPHA ~s ~s\n",
	                        [Number, escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec say_date(ChannelPid::pid(), Date::integer(),
%%                EscapeDigits::string()) -> {ok, Value}
%%                                           Value = integer()
%% @doc
%% Says the given <em>UnixTimeStamp</em> as a date.
%% <p>
%% <em>Date</em> is a Unix Timestamp, or, the number of seconds elapsed since
%% 00.00.00 on January 1, 1970.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0,  Playback completed without being interrupted by an escape digit</li>
%% <li><em>Value</em>, ASCII value of the digit pressed</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
say_date(ChannelPid, Date, EscapeDigits) ->
	Command = io_lib:format("SAY DATE ~B ~s\n",
	                        [Date, escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).
%% -----------------------------------------------------------------------------
%% @spec say_datetime(ChannelPid::pid(), UnixTimeStamp::integer(),
%%                    EscapeDigits::string()) ->  {ok, Value}
%%                                                Value = integer()
%% @doc
%% Says the given <em>UnixTimeStamp</em> as a datetime.
%% <p>
%% Says the given UnixTimeStamp, allowing playback to be interrupted if any of
%% the given <em>EscapeDigits</em> is pressed (empty string if none).
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0, Playback completed without being interrupted by an escape digit</li>
%% <li><em>value</em>, ASCII value of digit (if pressed)</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
say_datetime(ChannelPid, UnixTimeStamp, EscapeDigits) ->
	Command = io_lib:format("SAY DATETIME ~B ~s\n",
	                [round(UnixTimeStamp), escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec say_datetime(ChannelPid::pid(), UnixTimeStamp::integer(),
%%                    EscapeDigits::string(), Format::string()) -> {ok, Value}
%%                                                          Value = integer()
%% @doc
%% Says the given <em>UnixTimeStamp</em> as a datetime.
%% <p>
%% Behaves like {@link say_datetime/3} except the optional <em>Format</em>.
%% </p>
%% <p>
%% <em>Format</em> defaults to "ABdY 'digits/at' IMp" according to the following
%% format:<br />
%% <pre>
%% A or a        Day of week (Saturday, Sunday, ...)
%% B or b or h   Month name (January, February, ...)
%% d or e        numeric day of month (first, second, ..., thirty-first)
%% Y             Year
%% I or l        Hour, 12 hour clock
%% H             Hour, 24 hour clock (single digit hours preceded by "oh")
%% k             Hour, 24 hour clock (single digit hours NOT preceded by "oh")
%% M             Minute, with 00 pronounced as "o'clock"
%% N             Minute, with 00 pronounced as "hundred" (US military time)
%% P or p        AM or PM
%% Q             "today", "yesterday" or ABdY
%%               (*note: not standard strftime value)
%% q             "" (for today), "yesterday", weekday, or ABdY
%%               (*note: not standard strftime value)
%% R             24 hour time, including minute
%% </pre>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0, Playback completed without being interrupted by an escape digit</li>
%% <li><i>value</i>, ASCII value of digit (if pressed), in decimal</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
say_datetime(ChannelPid, UnixTimeStamp, EscapeDigits, Format) ->
	Command = io_lib:format("SAY DATETIME ~B ~s ~s\n",
               [round(UnixTimeStamp), escape_digits(EscapeDigits), quote(Format)]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec say_datetime(ChannelPid::pid(), UnixTimeStamp::integer(),
%%                    EscapeDigits::string(), Format::string(),
%%                    TimeZone::string()) ->  {ok, Value}
%%                                                          Value = integer()
%% @doc
%% Says the given <em>UnixTimeStamp</em> as a datetime.
%% <p>
%% Behaves like {@link say_datetime/4} except the it accepts the second optional
%% argument <em>TimeZone</em>.
%% </p>
%% <p>
%% Acceptable values for
%% timezone can be found in <em>/usr/share/zoneinfo</em>.
%% <em>timezone</em> defaults to the default time zone of the Asterisk server.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0, Playback completed without being interrupted by an escape digit</li>
%% <li><i>value</i>, ASCII value of digit (if pressed), in decimal</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
say_datetime(ChannelPid, UnixTimeStamp, EscapeDigits, Format, TimeZone) ->
	Command = io_lib:format("SAY DATETIME ~B ~s ~s ~s\n",
	      [round(UnixTimeStamp), escape_digits(EscapeDigits),
		   quote(Format), quote(TimeZone)]),
	integer_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec say_digits(ChannelPid::pid(), Number::string(),
%%                  EscapeDigits::string()) -> {ok, Value}
%%                                              Value = integer()
%%                                                   
%% @doc
%% Says a given number.
%% <p>
%% Says a given digit string, returning early if any of the given DTMF digits
%% are received on the channel.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0,  Playback completed without being interrupted by an escape digit</li>
%% <li><em>Value</em>, ASCII value of digit pressed</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
say_digits(ChannelPid, Number, EscapeDigits) ->
	Command = io_lib:format("SAY DIGITS ~s ~s\n",
	                        [Number, escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec say_number(ChannelPid::pid(), Number::integer(),
%%                  EscapeDigits::string()) -> {ok, Value}
%%                                           Value = integer()
%% @doc
%% Says a given number, returning early if any of the given DTMF digits are
%% received on the channel.
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0,  Playback completed without being interrupted by an escape digit</li>
%% <li><em>Value</em>, ASCII value of digit sent</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
say_number(ChannelPid, Number, EscapeDigits) ->
	Command = io_lib:format("SAY NUMBER ~B ~s\n",
	                        [Number, escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).


%% ----------------------------------------------------------------------------
%% @spec say_phonetic(ChannelPid::pid(), String::string(),
%%                    EscapeDigits::string()) -> {ok, Value}
%%                                               Value = integer()
%% @doc
%% Says a string with phonetics.
%% <p>
%% Say the <em>String</em> with phonetics, returning early if any of the given
%% DTMF digits are received on the channel.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li> 0, Playback completed without being interrupted by an escape digit</li>
%% <li><em>value</em>, ASCII value of digit sent</li>
%% </ul>
%% </p>
%% @end
%% ----------------------------------------------------------------------------
say_phonetic(ChannelPid, String, EscapeDigits) ->
	Command = io_lib:format("SAY PHONETIC ~s ~s\n",
	                        [quote(String), escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).

%% ----------------------------------------------------------------------------
%% @spec say_time(ChannelPid::pid(), Time::integer(),
%%                EscapeDigits::string()) -> {ok, Value}
%%                                           Value = integer()
%%
%% @doc
%% Says the given time.
%% <p>
%% Says the given <em>Time</em>, returning early if any of the DTMF digits in
%% <em>EscapeDigits</em> are received on the channel.
%% <em>Time</em> is a Unix Timestamp.
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li> 0, Playback completed without being interrupted by an escape digit</li>
%% <li><em>value</em>, ASCII value of the digit pressed</li>
%% </ul>
%% </p>
%% @end
% ----------------------------------------------------------------------------
say_time(ChannelPid, Time, EscapeDigits) ->
	Command = io_lib:format("SAY TIME ~B ~s\n",
	                        [Time, escape_digits(EscapeDigits)]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec send_image(ChannelPid::pid(), Image::string()) -> {ok, Value}
%%                                                 Value = integer()
%% @doc
%% Sends an image on the current channel.
%% <p>
%% <em>Image</em> is the filename of an image without it's extension.
%% </p>
%% <p>
%% Note: Most channels does not support sending images.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li>-1, Error or hangup</li>
%%     <li>0, Image sent, or channel does not support sending an image.</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
send_image(ChannelPid, Image) ->
	Command = io_lib:format("SEND IMAGE ~s\n", [Image]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec send_text(ChannelPid::pid(), Text::string()) -> {ok, Value}
%%                                                       Value = integer()
%% @doc
%% Sends text on the current channel.
%% <p>
%% <em>Text</em> is a string with the text to send.
%% </p>
%% <p>
%% Note: Most channels does not support sending text.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li>-1, Error or hangup</li>
%%     <li>0, Image sent, or channel does not support sending an image.</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
send_text(ChannelPid, Text) ->
	Command = io_lib:format("SEND TEXT ~s\n", [quote(Text)]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec set_auto_hangup(ChannelPid::pid, Time::integer()) -> {ok, 0}
%% @doc
%% Causes the channel to automatically be hung up once <em>Time</em> seconds have
%% elapsed.
%% <p>
%% Setting <em>Time</em> to 0 will disable auto hangup on the channel.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li>0, Auto hangup has been set</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_auto_hangup(ChannelPid, Time) ->
	Command = io_lib:format("SET AUTOHANGUP ~B\n", [Time]),
	integer_or_error(execute(ChannelPid, Command)).


%% -----------------------------------------------------------------------------
%% @spec set_callerid(ChannelPid::pid(), Number::string()) -> {ok, 1}
%% @doc
%% Changes the Caller ID of the current channel.
%% <p>
%% Return values:
%% <ul>
%%     <li>1, Caller ID has been set</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_callerid(ChannelPid, Number) ->
	Command = io_lib:format("SET CALLERID ~s\n", [Number]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec set_context(ChannelPid::pid(), Context::string()) -> {ok, 0}
%% @doc
%% Sets the context for continuation upon exiting the AGI application. 
%% <p>
%% Return values:
%% <ul>
%%     <li>0, Context has been set</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_context(ChannelPid, Context) ->
	Command = io_lib:format("SET CONTEXT ~s\n", [Context]),
	integer_or_error(execute(ChannelPid, Command)).


%% -----------------------------------------------------------------------------
%% @spec set_extension(ChannelPid::pid(), Extension::string()) -> {ok, 0}
%% @doc
%% Sets the extension for continuation upon exiting the AGI application.
%% <p>
%% Return values:
%% <ul>
%%     <li>0, extension has been set</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_extension(ChannelPid, Extension) ->
	Command = io_lib:format("SET EXTENSION ~s\n", [Extension]),
	integer_or_error(execute(ChannelPid, Command)).



%% -----------------------------------------------------------------------------
%% @spec set_music_on(ChannelPid::pid(), On::bool()) -> {ok, 0}
%% @doc
%% Enables/disables <em>Music on Hold</em> generator.
%% <p>
%% Will use the default Music on Hold class.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li>0, Music has been set</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_music_on(ChannelPid, On) ->
	set_music_on(ChannelPid, On, "").

%% -----------------------------------------------------------------------------
%% @spec set_music_on(ChannelPid::pid(), On::bool(), Class::string()) ->
%%                                                                {ok, 0}
%% @doc
%% Enables/disables <em>Music on Hold</em> generator, which a specific class.
%% <p>
%% Will use the Music on Hold class specified by <em>Class</em>.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li>0, Music has been set</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_music_on(ChannelPid, On, Class) ->
	OnArg = case On of
		true  -> "ON";
		false -> "OFF"
	end,
	Command = io_lib:format("SET MUSIC ~s ~s\n", [OnArg, Class]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec set_priority(ChannelPid::pid(), Priority::string()) -> {ok, 0}
%% @doc
%% Changes the priority for the continuation upon exiting the AGI application.
%% <p>
%% <em>Priority</em> must be a valid priority or label.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li></li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
set_priority(ChannelPid, Priority) ->
	Command = io_lib:format("SET PRIORITY ~s\n", [Priority]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec set_variable(ChannelPid::pid(), Name::string(), Value::string()) ->
%%                                                        {ok, 1}
%%                            
%% @doc
%% Sets a variable.
%% <p>
%% Sets variable with <em>Name</em> to <em>Value</em>.
%% </p>
%% Return values:
%% <ul>
%% <li>1, Success</li>
%% </ul>
%% @end
%% -----------------------------------------------------------------------------
set_variable(ChannelPid, Name, Value) ->
	Command = io_lib:format("SET VARIABLE ~s \"~s\"\n", [Name, Value]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec stream_file(ChannelPid::pid(), FileName::string(),
%%                   EscapeDigits::string()) -> {ok, {Value, EndPoint}}
%%                                   Value = integer()
%%                                   EndPoint = integer()
%% @doc
%% Plays an audio file, allowing playback to be interrupted.
%% <p>
%% Plays the audio file specified by <em>FileName</em>, allowing playback to be
%% interrupted by the digits specified by <em>EscapeDigits</em>, if any (empty
%% string if none).
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0,  Playback completed with no digit pressed</li>
%% <li><em>Value</em>,  ASCII value of digit (if pressed)</li>
%% </ul>
%% <em>EndPoint</em> refers to how long the file played for.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
stream_file(ChannelPid, FileName, EscapeDigits) ->
	Command = io_lib:format("STREAM FILE ~s ~s\n",
	                        [FileName, escape_digits(EscapeDigits)]),
	int_and_endpoint_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec stream_file(ChannelPid::pid(), FileName::string(),
%%                   EscapeDigits::string(), SampleOffset::integer()) ->
%%                                                      {ok, {Value, EndPoint}}
%%                                                       Value = integer()
%%                                                       EndPoint = integer()
%% @doc
%% Plays an audio file from a specific position, allowing playback to be
%% interrupted.
%% <p>
%% Plays the audio file specified by <em>FileName</em>, starting at
%% <em>SampleOffset</em> allowing playback to be
%% interrupted by the digits specified by <em>EscapeDigits</em>, if any (empty
%% string if none). 
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>-1, Error or hangup</li>
%% <li>0,  Playback completed with no digit pressed</li>
%% <li><em>Value</em>,  ASCII value of digit (if pressed), in decimal</li>
%% </ul>
%% <em>EndPoint</em> refers to how long the file played for.
%% </p>
%% @end
%% -----------------------------------------------------------------------------
stream_file(ChannelPid, FileName, EscapeDigits, SampleOffset) ->
	Command = io_lib:format("STREAM FILE ~s ~s ~B\n",
						[FileName, escape_digits(EscapeDigits), SampleOffset]),
	int_and_endpoint_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec tdd_mode(ChannelPid::pid(), Mode::bool()) -> {ok, Value}
%%                                                    Value = integer()
%% @doc
%% Enable/disable Telecommunications Devices for the Deaf (TDD)
%% transmission/reception on this channel.
%% <p>
%% Return values:
%% <ul>
%%     <li>0, Channel is not TDD-capable</li>
%%     <li>1, Success</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
tdd_mode(ChannelPid, Mode) ->
	ModeArg = case Mode of
		true  -> "ON";
		false -> "OFF"
	end,
	Command = io_lib:format("TDD MODE ~s\n", [ModeArg]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec verbose(ChannelPid::pid(), Message::string(), Level::integer()) -> AGIReturn
%% @doc
%% Sends a message to the console via the verbose system.
%% The <em>Level</em> argument is the minimum verbosity level at which the
%% <em>Message</em> will appear on the Asterisk command-line interface.
%% <p>
%% </p>
%% <p>
%% Return values:
%% <ul>
%% <li>0</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
verbose(ChannelPid, Message, Level) ->
	Command = io_lib:format("VERBOSE ~s ~B\n", [quote(Message), Level]),
	integer_or_error(execute(ChannelPid, Command)).

%% -----------------------------------------------------------------------------
%% @spec wait_for_digit(ChannelPid::pid(), Timeout::integer()) -> {ok, Value}
%%                                                         Value = integer()
%% @doc
%% Wait for the channel to receive a DTMF digit.
%% <p>
%% Will wait for <em>Timeout</em> milliseconds for the channel to receive a DTMF
%% digit. Use -1 for infinity.
%% </p>
%% <p>
%% Return values:
%% <ul>
%%     <li>-1, Error or hangup</li>
%%     <li>0, Timeout</li>
%%     <li><em>Value</em>, ASCII value of the digit received</li>
%% </ul>
%% </p>
%% @end
%% -----------------------------------------------------------------------------
wait_for_digit(ChannelPid, Timeout) ->
	Command = io_lib:format("WAIT FOR DIGIT ~B\n", [Timeout]),
	integer_or_error(execute(ChannelPid, Command)).

%%% ----------------------------------------------------------------------------
%%%                         Intermodule functions
%%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%% @private
%% @spec parse_init_arg(Binary::binary(), Record::record()) -> record()
%% @doc
%% Parses the initial data sent by Asterisk when a AGI channel is created.
%% @end
%% Types are taken from res/res_agi.c in the Asterisk sources.
%% ----------------------------------------------------------------------------
parse_init_arg(<<"agi_network: ", Var/binary>>, Record) ->
	Record#agi_env{network = eastrisk_types:to_bool(Var)};
parse_init_arg(<<"agi_request: ", Var/binary>>, Record) ->
	Record#agi_env{request = binary_to_list(Var)};
parse_init_arg(<<"agi_channel: ", Var/binary>>, Record) ->
	Record#agi_env{channel = binary_to_list(Var)};
parse_init_arg(<<"agi_language: ", Var/binary>>, Record) ->
	Record#agi_env{language = binary_to_list(Var)};
parse_init_arg(<<"agi_type: ", Var/binary>>, Record) ->
	Record#agi_env{type = binary_to_list(Var)};
parse_init_arg(<<"agi_uniqueid: ", Var/binary>>, Record) ->
	Record#agi_env{uniqueid = binary_to_list(Var)};
parse_init_arg(<<"agi_callerid: ", Var/binary>>, Record) ->
	Record#agi_env{callerid = binary_to_list(Var)};
parse_init_arg(<<"agi_calleridname: ", Var/binary>>, Record) ->
	Record#agi_env{calleridname = binary_to_list(Var)};
parse_init_arg(<<"agi_callingpres: ", Var/binary>>, Record) ->
	Record#agi_env{callingpres = list_to_integer(binary_to_list(Var))};
parse_init_arg(<<"agi_callingani2: ", Var/binary>>, Record) ->
	Record#agi_env{callingani2 = list_to_integer(binary_to_list(Var))};
parse_init_arg(<<"agi_callington: ", Var/binary>>, Record) ->
	Record#agi_env{callington = list_to_integer(binary_to_list(Var))};
parse_init_arg(<<"agi_callingtns: ", Var/binary>>, Record) ->
	Record#agi_env{callingtns = list_to_integer(binary_to_list(Var))};
parse_init_arg(<<"agi_dnid: ", Var/binary>>, Record) ->
	Record#agi_env{dnid = binary_to_list(Var)};
parse_init_arg(<<"agi_rdnis: ", Var/binary>>, Record) ->
	Record#agi_env{rdnis = binary_to_list(Var)};
parse_init_arg(<<"agi_context: ", Var/binary>>, Record) ->
	Record#agi_env{context = binary_to_list(Var)};
parse_init_arg(<<"agi_extension: ", Var/binary>>, Record) ->
	Record#agi_env{extension = binary_to_list(Var)};
parse_init_arg(<<"agi_priority: ", Var/binary>>, Record) ->
	Record#agi_env{priority = binary_to_list(Var)};
parse_init_arg(<<"agi_enhanced: ", Var/binary>>, Record) ->
	Record#agi_env{enhanced = binary_to_list(Var)};
parse_init_arg(<<"agi_accountcode: ", Var/binary>>, Record) ->
	Record#agi_env{accountcode = binary_to_list(Var)};
parse_init_arg(_, Record) -> Record.

%%% ----------------------------------------------------------------------------
%%%                              Internal functions                             
%%% ----------------------------------------------------------------------------

%% turns escape digits into empty double quotes if it is an empty string
escape_digits(EscapeDigits) when length(EscapeDigits) > 0 ->
	EscapeDigits;
escape_digits(_EscapeDigits) ->
	"\"\"".

%% -----------------------------------------------------------------------------
%% @spec execute(ChannelPid, Command) -> term()
%% @doc
%% Executes an AGI command and returns the result.
%% This function is used by the AGI commands and should not be accessed
%% directly.
%% See {@link interpret_result/1} for the return of this function.
%% @end
%% -----------------------------------------------------------------------------
execute(ChannelPid, Command) ->
	case agi_channel:send(ChannelPid, Command) of
		{ok, Data} ->
			interpret_result(Data);
		{error, closed} ->
			{error, {-1, "Channel closed"}};
		{error, ebadf} ->
			{error, {-1, "Channel closed"}}
	end.

%% -----------------------------------------------------------------------------
%% @spec interpret_result(Result) -> {ok, Return} | {error, {Code, Message}}
%%                Return = integer() | {Result, Value} | {Result, Value, Endpos}
%%                Result = string() | integer()
%%                Value = integer() | string() | atom()
%%                Endpos = integer()
%% @doc
%% Interprets the response from Asterisk.
%% @end
%% -----------------------------------------------------------------------------
interpret_result(Result) ->
	ReturnCode = return_code(Result),
	if
		ReturnCode == 200 ->
			{ok, return_value(Result)};
		true ->
			{error, {ReturnCode, error_msg(Result)}}
	end.

%% -----------------------------------------------------------------------------
%% @spec return_code(Binary::binary) -> integer()
%% @doc
%% Extracts a return code from an Asterisk AGI response
%% It does <u>not</u> handle 520 (Command syntax error), because I can't do that
%% without hacking the C code a lot. Also, the 520 should <u>not</u> happen.
%% @end
%% -----------------------------------------------------------------------------
return_code(<<"200", _Value/binary>>) ->
	200;
return_code(<<"510", _Value/binary>>) ->
	510.

%% Extracts a return value from an Asterisk AGI response
%% Usually returns an integer, but if the AGI commands returns a value in
%% parenthesis the value is returned as a string.
%% 200 [Rr]esult=Value
return_value(<<_Code:3/binary, _:7/binary , $=, Value/binary>>) ->
	ResString = binary_to_list(Value),
	List = split_return_string(ResString),
	case length(List) of
		1    -> hd(List);
		_Any -> list_to_tuple(List)
	end.

%% -----------------------------------------------------------------------------
%% @spec split_return_string(String::string()) -> list()
%% @doc
%% Splits a return string into a list of return values.
%% @end
%% -----------------------------------------------------------------------------
split_return_string(String) ->
	List = extract_endpos(String,
	               extract_value(String,
				                 extract_result(String))),
	lists:reverse(List).

%% -----------------------------------------------------------------------------
%% @spec extract_result(String::string()) -> list()
%% @doc
%% Returns the result (result=X) of a result string in a list.
%% @end
%% -----------------------------------------------------------------------------
extract_result(String) ->
	{match, 1, Len} = regexp:first_match(String, "^(-)?[0-9]+"),
	[string:substr(String, 1, Len)].

%% -----------------------------------------------------------------------------
%% @spec extract_value(String::string(), Acc::list()) -> list()
%% @doc
%% Adds the "value" argument (result=5 (X)) of a return string to an
%% accumulator.
%% @end
%% -----------------------------------------------------------------------------
extract_value(String, Acc) ->
	case regexp:first_match(String, "\\(.+\\)") of
		{match, Start, Len} ->
			[string:substr(String, Start + 1, Len - 2)|Acc];
		nomatch ->
			Acc
	end.

%% -----------------------------------------------------------------------------
%% @spec extract_endpos(String::string(), Acc::list()) -> list()
%% @doc
%% Adds the "endpos" (endpos=X) argument of a return string to an accumulator.
%% @end
%% -----------------------------------------------------------------------------
extract_endpos(String, Acc) ->
	case regexp:first_match(String, "endpos=[0-9]+") of 
		{match, Start, Len} ->
			[string:substr(String, Start + 7, Len - 7)|Acc];
		nomatch ->
			Acc
	end.

%% -----------------------------------------------------------------------------
%% @spec error_msg(Binary::binary()) -> string()
%% @doc
%% returns the error message from a AGI return string
%% @end
%% -----------------------------------------------------------------------------
error_msg(<<_:4/binary, Message/binary>>) ->
	binary_to_list(Message).

%% -----------------------------------------------------------------------------
%% @spec quote(String::string()) -> string()
%% @doc
%% Put quotes around a string.
%% I.e: <code>quote("Foo") -> "\Foo"\""</code>
%% @end
%% -----------------------------------------------------------------------------
quote(String) ->
	io_lib:format("\"~s\"", [String]).

%%% functions that change the returned strings in to the types expected
integer_or_error(Return) ->
	case Return of
		{ok, Value} ->
			{ok, list_to_integer(Value)};
		Else ->
			Else
	end.

int_and_endpoint_or_error(Return) ->
	case Return of
		{ok, {Value, Endpoint}} ->
			{ok, {list_to_integer(Value), list_to_integer(Endpoint)}};
		Else ->
			Else
	end.

int_and_atom_or_error(Return) ->
	case Return of
		{ok, {Value, Atom}} ->
			{ok, {Value, list_to_atom(Atom)}};
		Else ->
			Else
	end.

int_and_string_or_error(Return) ->
	case Return of
		{ok, {Value, Atom}} ->
			{ok, {list_to_integer(Value), Atom}};
		Else ->
			Else
	end.

int_tuple_int_or_error(Return) ->
	case Return of
		{ok, {Result, Atom, Endpoint}}  ->
			{ok, {list_to_integer(Result),
			      list_to_atom(Atom),
				  list_to_integer(Endpoint)}};
		Else ->
			Else
	end.

%%% ----------------------------------------------------------------------------
%%%                           Type definitions
%%% ----------------------------------------------------------------------------
%%%
%%% @type agi_error(Code, Message) = {integer(), string()}.
%%% Error codes:<br />
%%% 510 = AGI Illegal command<br />
%%% -1 = Channel closed <br />
%%% @end
%%% ----------------------------------------------------------------------------
