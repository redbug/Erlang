-module(message_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, register_nick/2, unregister_nick/1, send_chat_message/2, 
	 shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% Client API
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

register_nick(ClientName, ClientPid) ->
    gen_server:call({global, ?SERVER}, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
    gen_server:call({global, ?SERVER}, {unregister_nick, ClientName}).

send_chat_message(Addressee, MessageBody) ->
    gen_server:call({global, ?SERVER}, {send_chat_msg, Addressee, MessageBody}).

shutdown() ->
    gen_server:cast({global, ?SERVER}, stop).

%% gen_server callbacks

init([]) ->
    % start by message_router_sup instead
    %message_store:start_link(),
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    {ok, dict:new()}.

handle_call({register_nick, ClientName, ClientPid}, _From, Clients) ->
    Messages = message_store:find_messages(ClientName),
    lists:foreach(fun(Msg) -> ClientPid ! {printmsg, Msg} end, Messages),
    {reply, ok, dict:store(ClientName, ClientPid, Clients)};

handle_call({unregister_nick, ClientName}, _From, Clients) ->
    UpdatedClients = case dict:find(ClientName, Clients) of
			{ok, ClientPid} ->
			    ClientPid ! stop,
			    dict:erase(ClientName, Clients);
			error ->
			    io:format("Error! Unknown client: ~p~n", [ClientName]),
			    Clients
		      end,
    {reply, ok, UpdatedClients};

handle_call({send_chat_msg, ClientName, MessageBody}, _From, Clients) ->
    case dict:find(ClientName, Clients) of
	{ok, ClientPid} ->
	    ClientPid ! {printmsg, MessageBody};
	error ->
            message_store:save_message(ClientName, MessageBody),
	    io:format("Archived message for ~p~n", [ClientName])
    end,
    {reply, ok, Clients};			

handle_call(_Request, _From, State) ->
    Reply =ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    %message_store:shutdown(),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

