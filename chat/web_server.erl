
-module(web_server).
-define(OK, <<"ok">>).
-export([start/1, stop/0, dispatch_requests/1]).

start(Port) ->
    mochiweb_http:start([{port, Port},
			 {loop, fun dispatch_requests/1}]).

stop() ->
    mochiweb_http:stop().

% url dispatcher
dispatch_requests(Req) ->
    Path = Req:get(path),
    Action = clean_path(Path),
    handle(Action, Req).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% controller (views.py in django)
% 1. fetch url query
% 2. communicate with mucc server
% 3. respond to client
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle("/send", Req) ->
    Params = Req:parse_qs(),
    Sender = proplists:get_value("nick", Params),
    Addressee = proplists:get_value("to", Params),
    Message = proplists:get_value("msg", Params),
    mucc:send_message(Sender, Addressee, Message),
    success(Req, ?OK);

handle("/poll", Req) ->
    Params = Req:parse_qs(),
    Nickname = proplists:get_value("nick", Params),
    case mucc:poll(Nickname) of
	{error, Error} ->
	    error_500(Req, subst("Error: ~s~n", Error));
	Messages ->
	    case length(Messages) == 0 of
		true ->
		    success(Req, <<"none">>);
		false ->
		    Template = lists:foldl(fun(_, Acc) -> ["~s~n"|Acc] end, [], Messages),
		    success(Req, subst(lists:flatten(Template), Messages))
	    end
    end;

handle("/register", Req) ->
    Params = Req:parse_qs(),
    Nickname = proplists:get_value("nick", Params),
    case mucc:register_nickname(Nickname) of
	ok ->
	    success(Req, ?OK);
	Error ->
	    error_500(Req, subst("Error: ~s", [Error]))
    end;

handle(Unknown, Req) ->
    Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s", [Unknown])}).

error_500(Req, Body) when is_binary(Body) ->
    Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
    Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

subst(Template, Values) when is_list(Values) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
    case string:str(Path, "?") of
	0 ->
	    Path;
	N ->
	    string:substr(Path, 1, string:len(Path) - (N + 1 ))
    end.
