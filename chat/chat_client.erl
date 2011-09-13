-module(chat_client).
-compile(export_all).

register_nickname(Nickname) ->
    Pid = spawn(chat_client, handle_message, [Nickname]),
    message_router:register_nick(Nickname, Pid).

unregister_nickname(Nickname) ->
    message_router:unregister_nick(Nickname).

send_message(Addressee, MessageBody) ->
    message_router:send_chat_message(Addressee, MessageBody).

handle_message(Nickname) ->
    receive
	{printmsg, MessageBody} ->
	    io:format("~p received: ~p~n", [Nickname, MessageBody]),
	    handle_message(Nickname);

	stop ->
	    ok
    end.

start_router() ->
    message_router:start_link().
