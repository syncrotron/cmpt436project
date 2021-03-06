%%%Created by Sam Horovatin, 11185403

-module(messagehandler).
-include("../include/message.hrl").
-include("../include/domaintable.hrl").
-behaviour(gen_server).
% interface calls
-export([start/0, stop/0, pass_message/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).


%% ====================================================================
%% Server Interface
%% ====================================================================
%% Booting server (and linking to it)

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

stop() ->
    gen_server:cast(?MODULE, shutdown).

init(_Args) ->
    io:format("Initializing Satellite Message Handler...~n"),
    %io:format("Pid Of Domain Table: ~s~n", pidofdomaintable),
    %io:format("Pid Of Encoder Master: ~s~n", pidofencodermaster),
    io:format("~n..........................................~n"),
    %{Mega,Sec,Micro} = get_timestamp(),
    %File = Mega++Sec++Micro++".txt",
    %File = integer_to_list(get_timestamp())++".txt",
    %file:write_file("/doc/runlogs/" ++ File , ""),
    %error_logger:logfile({open, "/doc/runlogs/" ++ File}),
    {ok, self()}.

%% @doc Internal timestamp generator for error error_logger
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

%% ====================================================================
%% Server Functions
%% ====================================================================

pass_message(Msg) when is_record(Msg, message)->
    gen_server:cast(?MODULE, {message, Msg});

pass_message(UnkownMsg)->
    ok.
    %error_logger:error_msg("Unkown Message Passed: ~p~n", [UnkownMsg]).

%% @doc Main Handler. Should not be used outside module
handle_cast({message, Msg}, _) ->
    %error_logger:info_msg("Recieved Message From ~p~n
    %                       Sequence Number ~p in ~p~n
    %                       Headed to ~p~n"
    %                       , [Msg#message.sourceid, element(1, Msg#message.sequence), element(2, Msg#message.sequence), Msg#message.destination]),

    %% updates domain table entry with sender info
    MsgObject = #object{id=Msg#message.senderid,
                        position=Msg#message.senderposition,
                        delta_pos= x,
                        last_msg_t_stamp=get_timestamp()},
    pidOfdomaintable ! {insert_object, MsgObject},

    %% Checks file type and looks for resend requests or recieve confrimations
    FileType = Msg#message.ftype,
    if FileType =:= "RSEND" ->
         RepeatMsg = messagestore:get_message(Msg#message.sourceid),
         pidOfdomaintable ! {route, {position, Msg#message.senderposition}, []},
         pidofencodermaster ! {Msg};

       FileType =:= "OK" ->
         messagestore:flag_message_for_deletion(Msg#message.sourceid);

       FileType =:= "PING" ->
         ok;

       true ->
            %% Checks sequence info, and sends if entire sequence is collected
            if element(2, Msg#message.sequence) > 1 ->
                  messagestore:add_message(Msg),
                  {Truth, MessageSequence} = messagestore:has_complete_message(Msg#message.sourceid, Msg#message.request),
                  if Truth == true ->
                    OkMsg = #message{sourceid = Msg#message.sourceid,
                                        sourceposition = Msg#message.sourceposition,
                                        senderid = <<>>,
                                        senderposition = {0,0,0},
                                        sequence = {0,0},
                                        request = Msg#message.request,
                                        ftype = <<"OK">>,
                                        destination = {0,0,0},
                                        body = <<>>},
                    pidOfdomaintable ! {route, {position, Msg#message.destination}, [], Msg#message.request},
                    pidofencodermaster ! OkMsg
                  end;
               true ->
                 pidOfdomaintable ! {route, {position, Msg#message.senderposition}, [], Msg#message.request},
                 pidofencodermaster ! Msg
            end

    end,
    {noreply, ?MODULE}.


% We get compile warnings from gen_server unless we define these
handle_call(_Call, _From, State) -> {noreply, State}.
handle_info(_Message, Args) -> {noreply, Args}.
terminate(_Reason, _Args) -> ok.
code_change(_OldVersion, Args, _Extra) -> {ok, Args}.
