-module(gen_server_skeleton).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%% Client API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({save_msg, Addressee, Body}, _From, State) ->
    Reply =ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

