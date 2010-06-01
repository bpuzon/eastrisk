%%% ----------------------------------------------------------------------------
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%%
%%% @version 0.2, 2006-08-02
%%% @copyright 2006 Erlang Training and Consulting
%%% @doc
%%% Record for AGI interface.
%%% @end
%%% ----------------------------------------------------------------------------
-record(agi_env,
        {
         network,      % bool()
         request,      % string()
         channel,      % string()
         language,     % string()
         type,         % string()
         uniqueid,     % string()
         callerid,     % string()
         calleridname, % string()
         callingpres,  % integer()
         callingani2,  % integer()
         callington,   % integer()
         callingtns,   % integer()
         dnid,         % string()
         rdnis,        % string()
         context,      % string()
         extension,    % string()
         priority,     % string()
         enhanced,     % string()
         accountcode   % string()
        }).
