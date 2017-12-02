%%%Created by Sam Horovatin, 11185403

-module(messagehandler).
-include("../include/message.hrl").
-behaviour(gen_server).
% interface calls
-export([start/1, stop/0, pass_message/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).


%% ====================================================================
%% Server Interface
%% ====================================================================
%% Booting server (and linking to it)
%% Args {PidOfDomainTable, PidOfStorageTable, PidOfEncoderMaster}

start(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

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

%% ====================================================================
%% Server Functions
%% ====================================================================

pass_message(Msg) when is_record(Msg, message)->
    gen_server:cast(?MODULE, {message, Msg});

pass_message(UnkownMsg)->
    error_logger:error_msg("Unkown Message Passed: ~p~n", [UnkownMsg]).

%% @doc Main Handler. Should not be used outside module
handle_cast({message, Msg}, {PidOfDomainTable, PidOfStorageTable, PidOfEncoderMaster}) ->
    error_logger:info_msg("Recieved Message From ~p~n
                           Sequence Number ~p in ~p~n
                           Headed to ~p~n"
                           , [Msg#message.sourceid, element(1, Msg#message.sequence), element(2, Msg#message.sequence), Msg#message.destination]),

    %% Checks sequence info, and sends if entire sequence is collected
    FileType = Msg#message.ftype,
    if FileType =:= "RSP" ->
        Body

        true -> ok %%FUNCTION FOR SENDING TO ENCODER
    end,

    %% Checks sequence info, and sends if entire sequence is collected
    {Position, Sequence} = Msg#message.sequence,
    if Sequence > 1 ->
        messagestore:add_message(Msg),
        {Truth, MessageSequence} = messagestore:has_complete_message(Msg#message.sourceid),
        if Truth == true ->
            mass_send(MessageSequence, length(MessageSequence))
        end;
        true -> ok %%FUNCTION FOR SENDING TO ENCODER
    end,
    {noreply, {PidOfDomainTable, PidOfStorageTable, PidOfEncoderMaster}}.

mass_send(MessageSequence, SequenceNumber) -> ok  %%FUNCTION FOR SENDING TO ENCODER.
% We get compile warnings from gen_server unless we define these
handle_call(_Call, _From, State) -> {noreply, State}.
handle_info(_Message, Args) -> {noreply, Args}.
terminate(_Reason, _Args) -> ok.
code_change(_OldVersion, Args, _Extra) -> {ok, Args}.
