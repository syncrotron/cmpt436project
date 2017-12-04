%% @author Fredrik Johansson

-module(messagestore).
-include("../include/message.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).
-export([start/0, get_message/1, add_message/1, flag_message_for_deletion/1, has_complete_message/1, clean_up/0, clean_up_loop/0]).

-record(item, {deletion_flag = false, message_sequence}).

%% @doc Start the proccess which deal with the Messages Storage
start() ->
    io:fwrite("Started message store~n"),
    spawn(messagestore, clean_up_loop, []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Internal function (should not be used outisde of module). See start() for outisde ussage
init([]) ->
    Store = dict:new(),
    {ok, Store}.

clean_up_loop() ->
    receive
        after 5*60*1000 -> % 5 min
            clean_up(),
            io:fwrite("now~n"),
            clean_up_loop()
    end.

%% @doc Internal function (should not be used outisde of module). See add_message() for outisde ussage
handle_cast({add_message, Message}, Store) ->
    NewStore = case dict:find(Message#message.sourceid, Store) of
        {ok, Previous} ->
            UpdatedItem = Previous#item{message_sequence=[Message] ++ Previous#item.message_sequence},
            dict:store(Message#message.sourceid, UpdatedItem, Store);
        error ->
            dict:store(Message#message.sourceid, #item{message_sequence=[Message]}, Store)
    end,
    {noreply, NewStore};

%% @doc Internal function (should not be used outisde of module). See flag_message_for_deletion() for outisde ussage
handle_cast({flag_message_for_deletion, Id}, Store) ->
    NewStore = case dict:find(Id, Store) of
        {ok, Item} -> dict:store(Id, Item#item{deletion_flag=true}, Store);
        error -> Store
    end,
    {noreply, NewStore};

handle_cast({clean_up}, Store) ->
    NewStore = dict:filter(fun(_Id, Item) -> Item#item.deletion_flag == false end, Store),
    {noreply, NewStore}.

%% @doc Internal function (should not be used outisde of module). See get_message() for outisde ussage
handle_call({get_message, Id}, _From, Store) ->
    case dict:find(Id, Store) of
        {ok, Item} ->
            Sorted = sort_message_parts(Item#item.message_sequence),
            {reply, Sorted, Store};
        error -> {reply, nosuchitem, Store}
    end;

%% @doc Internal function (should not be used outisde of module). See has_complete_message() for outisde ussage
handle_call({has_complete_message, Id}, _From, Store) ->
    case dict:find(Id, Store) of
        {ok, Item} ->
            Last = lists:last(Item#item.message_sequence),
            Sorted = sort_message_parts(Item#item.message_sequence),
            {_Number, SequenceLength} = Last#message.sequence,
            if
                length(Item#item.message_sequence) == SequenceLength ->
                    {reply, {true, Sorted}, Store};
                true ->
                    {reply, {false, Sorted}, Store}
            end;
        error -> {reply, nosuchitem, Store}
    end.


sort_message_parts(List) ->
    lists:sort(
        fun(A, B) ->
            {N1, _M1} = A#message.sequence,
            {N2, _M2} = B#message.sequence,
             N1 < N2
        end, List).

%% @doc Add a partial message to the store
add_message(Message) ->
    gen_server:cast(messagestore, {add_message, Message}).

%% @doc Flag a message for deletion. When deletion is performed will not be known
flag_message_for_deletion(Id) ->
    gen_server:cast(messagestore, {flag_message_for_deletion, Id}).

%% @doc Get all message parts based on id. Will return a list of the
%%      message part or atom nosuchitem if the id doesn't match any messages
get_message(Id) ->
    gen_server:call(messagestore, {get_message, Id}).

%% @doc Check to see if a whole message has been stored. Returns either
%%      {true, <list of message parts>}, {false, <list of message parts>}
%%      or atom nosuchitem
has_complete_message(Id) ->
    gen_server:call(messagestore, {has_complete_message, Id}).

clean_up() ->
    gen_server:cast(messagestore, {clean_up}).


terminate(_Reason, _State) -> io:fwrite("Stopped message store~n").
handle_info(_Message, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
