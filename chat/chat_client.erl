-module(chat_client).
-compile(export_all).

register_nickname(Nickname) ->
    message_router:register_nick(Nickname, fun(Msg) -> print_message(Nickname, Msg) end).

unregister_nickname(Nickname) ->
    message_router:unregister_nick(Nickname).

send_message(Addressee, MessageBody) ->
    message_router:send_chat_message(Addressee, MessageBody).

print_message(Who, MessageBody) ->
    io:format("~p received: ~p~n", [Who, MessageBody]).

start_router() ->
    message_router:start().
% c(chat_client.erl).
% chat_client:send_message(P, P, "Hello from a client!").
% P1 = spawn(message_router, route_messages, []).
% chat_client:send_message(P, P1, "Hello from client on message router #1").
