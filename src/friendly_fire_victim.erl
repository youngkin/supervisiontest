%% @author uyounri
%% @doc Demonstrates that an otherwise innocent process will be terminated
%%		when its supervisor exits due to reached_max_restart_intensity.  It is
%%		indeed a friendly fire victim or perhaps an innocent bystander.


-module(friendly_fire_victim).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, ping/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

ping() ->
    gen_server:call(?MODULE, ping).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	lager:emergency("******************* friendly_fire_victim: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:emergency("******************* friendly_fire_victim: INIT~n", []),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(ping, _From, State) ->
    lager:emergency("******************* friendly_fire_victim: PING~n", []),
    {reply, {ok, pong}, State};

handle_call(crash_hello, _From, State) ->
    lager:emergency("******************* friendly_fire_victim: CRASH!!!~n", []),
	Result = suptest_hello_world:crash(),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	lager:emergency("******************* friendly_fire_victim: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.