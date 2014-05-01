%% @author uyounri
%% @doc Demonstrates that a service under supervision tree "A" isn't affected
%%      by failures in supervision tree "B".  
%%
%%		This module also demonstrates initializing an ets table during init/1.
%%		This is done to investigate/confirm ets definition and population isn't
%%		a problem during init/1.  For this purpose overall module state includes
%%		the contents of the ets table.  It can't be modified except for the value
%%		associated with current_state (which is State). get_state/0 returns 
%%		everything included in the ets table.
%%
%%      The "crash" function provides a hook to force it to crash if desired.
%% 


-module(non_std_child1).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, ping/0, crash/0, set_state/1, get_state/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
set_state(TheNewState) ->
    gen_server:call(?MODULE, {set_state, TheNewState}).
	
get_state() ->
    gen_server:call(?MODULE, get_state).
	
ping() ->
    gen_server:call(?MODULE, ping).

crash() ->
    gen_server:call(?MODULE, crash).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	lager:emergency("******************* non_std_child1: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:emergency("******************* non_std_child1: INIT~n", []),
	ets:new(?MODULE, [named_table, set, protected]),
	populate_table(),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call({set_state, TheNewState}, _From, _State) ->
    lager:emergency("******************* non_std_child1: set_state(~p)~n", [TheNewState]),
	true = ets:insert(?MODULE, {current_state, TheNewState}),
    {reply, {ok, pong}, TheNewState};

handle_call(get_state, _From, _State) ->
    lager:emergency("******************* non_std_child1: get_state()~n", []),
	CurrentState = [ets:lookup(?MODULE, X) || X <- [key1, key2, key3, key4, key5, key6, key7, key8, key9, key10, current_state]],
    {reply, {ok, CurrentState}, _State};

handle_call(ping, _From, State) ->
    lager:emergency("******************* non_std_child1: PING~n", []),
    {reply, {ok, pong}, State};

handle_call(crash, _From, State) ->
    lager:emergency("******************* non_std_child1: CRASH!!!~n", []),
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
	lager:emergency("******************* non_std_child1: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Helper functions
%% ====================================================================

populate_table() ->
	true = ets:insert(?MODULE, {key1, "Value-1"}),
	true = ets:insert(?MODULE, {key2, "Value-2"}),
	true = ets:insert(?MODULE, {key3, "Value-3"}),
	true = ets:insert(?MODULE, {key4, "Value-4"}),
	true = ets:insert(?MODULE, {key5, "Value-5"}),
	true = ets:insert(?MODULE, {key6, "Value-6"}),
	true = ets:insert(?MODULE, {key7, "Value-7"}),
	true = ets:insert(?MODULE, {key8, "Value-8"}),
	true = ets:insert(?MODULE, {key9, "Value-9"}),
	true = ets:insert(?MODULE, {key10, "Value-10"}).

