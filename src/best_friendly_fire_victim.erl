%% @author uyounri
%% @doc Demonstrates that a service under supervision tree "A" isn't affected
%%      by failures in supervision tree "B". Also demonstrates that process
%%		failures in the same supervision tree won't affect the availability of
%%		this gen_server.  It is "best" in the sense that it is somewhat immune
%%		to friendly fire by virtue of being supervised by a supervisor that has
%%		protective restart semantics.
%%
%%      The "crash" function provides a hook to force it to crash if desired.
%% 


-module(best_friendly_fire_victim).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, ping/0, crash/0, x/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

ping() ->
    gen_server:call(?MODULE, ping).

crash() ->
    gen_server:call(?MODULE, crash).

x() ->
	io:format("******************** x~n)", []).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	io:format("******************* best_friendly_fire_victim: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("******************* best_friendly_fire_victim: INIT~n", []),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(ping, _From, State) ->
    io:format("******************* best_friendly_fire_victim: PING~n", []),
    {reply, {ok, pong}, State};

handle_call(crash, _From, State) ->
    io:format("******************* best_friendly_fire_victim: CRASH!!!~n", []),
	X=1,
    {reply, X=2, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("******************* best_friendly_fire_victim: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.