-module(message_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("stdlib/include/qlc.hrl").
-record(state, {}).

-record(chat_message,
        {addressee,
	 body,
	 created_on}).

%% API
-export([start_link/0, save_message/2, find_messages/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Client API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_message(Addressee, Body) ->
    gen_server:call(?SERVER, {save_msg, Addressee, Body}).

find_messages(Addressee) ->
    case gen_server:call(?SERVER, {find_msgs, Addressee}) of
	{ok, Messages} ->
	    Messages
    end.

shutdown() ->
    gen_server:call(?SERVER, stop).

%% gen_server callbacks

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    init_store(),
    {ok, #state{}}.

handle_call({save_msg, Addressee, Body}, _From, State) ->
    store_message(Addressee, Body),
    {reply, ok, State};

handle_call({find_msgs, Addressee}, _From, State) ->
    Messages = get_messages(Addressee),
    {reply, {ok, Messages}, State};

handle_call(stop, _From, State) ->
    mnesia:stop(),
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, ignored_message, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Internal functions

get_messages(Addressee) ->
    F = fun() ->
	Query = qlc:q([ M#chat_message.body || M <- mnesia:table(chat_message),
			M#chat_message.addressee =:= Addressee]),
	Results = qlc:e(Query),
	delete_messages(Results),
	Results end,
    {atomic, Messages} = mnesia:transaction(F),
    Messages.

store_message(Addressee, Body) ->
    F = fun() ->
	{_, CreatedOn, _} = erlang:now(),
	mnesia:write(#chat_message{addressee=Addressee, body=Body, created_on=CreatedOn}) end,
    mnesia:transaction(F).

delete_messages(Messages) ->
     F = fun() ->
	lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages) end,
    mnesia:transaction(F).

init_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try 
	mnesia:table_info(chat_message, type) 
    catch                                       
	exit: _ ->                              
	    mnesia:create_table(chat_message, [ {attributes, record_info(fields, chat_message)},
						{type, bag},
						{disc_copies, [node()]}])
    end.

