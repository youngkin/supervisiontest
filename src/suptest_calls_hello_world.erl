%% @author uyounri
%% @doc Demonstrates that a call to another gen_server that throws an
%%      exception will propagate the exception to the calling server and,
%%		in this case (no exception handling), will cause it to crash.
%%


-module(suptest_calls_hello_world).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, say_hello/0, crash_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

say_hello() ->
    gen_server:call(?MODULE, hello).

crash_hello() ->
	gen_server:call(?MODULE, crash_hello).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	lager:emergency("******************* suptest_calls_hello_world: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:emergency("******************* suptest_calls_hello_world: INIT~n", []),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(hello, _From, State) ->
    lager:emergency("******************* suptest_calls_hello_world: HELLO~n", []),
	Result = suptest_hello_world:say_hello(),
    {reply, {ok, Result}, State};

handle_call(crash_hello, _From, State) ->
    lager:emergency("******************* suptest_calls_hello_world: CRASH!!!~n", []),
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
	lager:emergency("******************* suptest_calls_hello_world: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.