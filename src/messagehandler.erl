%%%Created by Sam Horovatin, 11185403

-module(messagehandler).

-behaviour(gen_server).
% interface calls
-export([start/0, stop/0]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



%% Server interface
%%====================================================================
%% Booting server (and linking to it)
%% Args {PidOfDomainTable, PidOfStorageTable, PidOfEncoderMaster}
start(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%Stops the server
stop() ->
    gen_server:cast(?MODULE, shutdown).

init({PidOfDomainTable, PidOfStorageTable, PidOfEncoderMaster}) ->
    io:format("Initializing Satellite Message Handler...~n"),
    io:format("Run Paramaters Given Are:~n"),
    io:format("Pid Of Domain Table:~s~n", PidOfDomainTable),
    io:format("Pid Of Storage Table:~s~n", PidOfStorageTable),
    io:format("Pid Of Encoder Master:~s~n", PidOfEncoderMaster),
    io:format("~n..........................................~n"),
    {Mega,Sec,Micro} = get_timestamp(),
    File = Mega++Sec++Micro++".txt",
    file:write_file("/doc/runlogs/" ++ File , ""),
    error_logger:logfile({open, "/doc/runlogs/" ++ File}),
    {ok, {PidOfDomainTable, PidOfStorageTable, PidOfEncoderMaster}}.


get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

%%Interface
%%====================================================================

pass_message(Msg) when is_record(Msg, message)->
    gen_server:cast(?MODULE, {message, Msg}).

pass_message(UnkownMsg)->
    error_logger:info_msg("Unkown Message Passed: ~p~n", [UnkownMsg]).

%% Synchronous, possible return values
% {reply,Reply,NewState}
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState}
% {stop,Reason,NewState}
handle_call(Message, From, State) ->
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

%% Asynchronous message manager
handle_cast({message, Msg}, Args) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {noreply, Args};

%% Informative calls
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info(_Message, _Server) ->
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

%% Server termination
terminate(_Reason, _Server) ->
    io:format("Terminating...~n").


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
