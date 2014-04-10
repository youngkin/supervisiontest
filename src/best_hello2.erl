%% @author uyounri
%% @doc Provides generic success and failure semantics that can be used to
%%      verify client exception handling and/or supervision restart strategies.
%%
%%      say_hello() and crash_hello() call across supervision trees.  For 
%%		crash_hello(), this results in the called process, in the other
%%      supervision tree, to crash - hmmmmmmm.  Probably best to avoid this
%%		situation wherever possible.
%%


-module(best_hello2).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([say_hello/0, crash_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

say_hello() ->
    gen_server:call(?MODULE, hello).

crash_hello() ->
	gen_server:call(?MODULE, crash_hello).

%% ====================================================================
%% Internal functions
%% ====================================================================

init([]) ->
	io:format("******************* best_hello2: INIT~n", []),
    {ok, [], 1000}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(hello, _From, State) ->
    io:format("******************* best_hello2: HELLO~n", []),
	Result = suptest_hello_world:say_hello(),
    {reply, {ok, Result}, State};

handle_call(crash_hello, _From, State) ->
    io:format("******************* best_hello2: CRASH!!!~n", []),
	Result = suptest_hello_world:crash(),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({From, say_hello}, State) ->
    io:format("******************* best_hello2:hendle_info(SAY_hell0) from PID ~p!!!~n", [self()]),	
	Result = suptest_hello_world:say_hello(),
	From ! {say_hello_response, Result},
    {noreply, State};
handle_info({_From, crash_hello}, State) ->
    io:format("******************* best_hello2:hendle_info(CRASH_hell0)!!!~n", []),	
	_Result = suptest_hello_world:crash(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("******************* best_hello2: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.