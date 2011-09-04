-module(message_router).
-define(SERVER, message_router).
-compile(export_all).

start() ->
    Pid = spawn(message_router, route_messages, [dict:new()]),
    erlang:register(?SERVER, Pid),
    Pid.

stop() ->
    ?SERVER ! shutdown.

register_nick(ClientName, ClientPid) ->
    ?SERVER ! {register_nick, ClientName, ClientPid}.

unregister_nick(ClientName) ->
    ?SERVER ! {unregister_nick, ClientName}.

send_chat_message(Addressee, MessageBody) ->
    ?SERVER ! {send_chat_msg, Addressee, MessageBody}.

route_messages(Clients) ->
    receive
	{send_chat_msg, ClientName, MessageBody} ->
	    ?SERVER ! {recv_chat_msg, ClientName, MessageBody},
	    route_messages(Clients);

	{recv_chat_msg, ClientName, MessageBody} ->
	    case dict:find(ClientName, Clients) of
		{ok, ClientPid} ->
		    ClientPid ! {printmsg, MessageBody};
		error ->
		    io:format("Error! Unknown client: ~p~n", [ClientName])
	    end,

	    route_messages(Clients);

	{register_nick, ClientName, ClientPid} ->
	    route_messages(dict:store(ClientName, ClientPid, Clients));

	{unregister_nick, ClientName} ->
	    case dict:find(ClientName, Clients) of
		{ok, ClientPid} ->
		    ClientPid ! stop,
		    route_messages(dict:erase(ClientName, Clients));
		error ->
		    io:format("Error! Unknown client: ~p~n", [ClientName]),
		    route_messages(Clients)
	    end;

	shutdown ->
	    io:format("Shutting down!~n");

	Oops ->
	    io:format("Warning! Receive: ~p~n", [Oops]),
	    route_messages(Clients)
    end.


%% Testing
%% c(message_router.erl).
%% P = spawn(message_router, route_messages,  []).
%% P ! {send_chat_msg, P, "Hello, World!"}.   % send P itself a message.
%% P ! {huh}.

