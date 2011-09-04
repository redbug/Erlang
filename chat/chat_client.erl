-module(chat_client).
-compile(export_all).

send_message(RouterPid, Addressee, MessageBody) ->
    message_router:send_chat_message(RouterPid, Addressee, MessageBody).

print_message(MessageBody) ->
    io:format("Received: ~p~n", [MessageBody]).

start_router() ->
    message_router:start(fun chat_client:print_message/1).
% c(chat_client.erl).
% chat_client:send_message(P, P, "Hello from a client!").
% P1 = spawn(message_router, route_messages, []).
% chat_client:send_message(P, P1, "Hello from client on message router #1").
