-module(message_router).

-compile(export_all).

start(PrintFun) ->
    spawn(message_router, route_messages, [PrintFun]). 

stop(RouterPid) ->
    RouterPid ! shutdown.

send_chat_message(RouterPid, Addressee, MessageBody) ->
    RouterPid ! {send_chat_msg, Addressee, MessageBody}.

route_messages(PrintFun) ->
    receive
	{send_chat_msg, Addressee, MessageBody} ->
	    Addressee ! {recv_chat_msg, MessageBody},
	    route_messages(PrintFun);
	{recv_chat_msg, MessageBody} ->
	    PrintFun(MessageBody),
	    route_messages(PrintFun);
	shutdown ->
	    io:format("Shutting down!~n");
	Oops ->
	    io:format("Warning! Receive: ~p~n", [Oops]),
	    route_messages(PrintFun)
    end.


%% Testing
%% c(message_router.erl).
%% P = spawn(message_router, route_messages,  []).
%% P ! {send_chat_msg, P, "Hello, World!"}.   % send P itself a message.
%% P ! {huh}.

