%% @author uyounri
%% @doc Demonstrates that a service under supervision tree "A" isn't affected
%%      by failures in supervision tree "B".
%%
%%      The "crash" function provides a hook to force it to crash if desired.
%% 


-module(non_std_child2).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, ping/0, crash/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
ping() ->
    gen_server:call(?MODULE, ping).

crash() ->
    gen_server:call(?MODULE, crash).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	lager:emergency("******************* non_std_child2: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:emergency("******************* non_std_child2: INIT~n", []),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(ping, _From, State) ->
    lager:emergency("******************* non_std_child2: PING~n", []),
    {reply, {ok, pong}, State};

handle_call(crash, _From, State) ->
    lager:emergency("******************* non_std_child2: CRASH!!!~n", []),
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
	lager:emergency("******************* non_std_child2: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.