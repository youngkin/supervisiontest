%% @author uyounri
%% @doc A simple gen_server that accepts requests to say "hi" or to crash on
%%		demand.


-module(suptest_hello_world).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, say_hello/0, crash/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

say_hello() ->
    gen_server:call(?MODULE, hello).

crash() ->
	gen_server:call(?MODULE, crash).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	io:format("******************* suptest_hello_world: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("******************* suptest_hello_world: INIT~n", []),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(hello, _From, State) ->
    io:format("******************* suptest_hello_world: HELLO~n", []),
    {reply, {ok, "Hi!"}, State};

handle_call(crash, _From, State) ->
    io:format("******************* suptest_hello_world: CRASH!!!~n", []),
	X=1,
    {reply, X=2, State};  %% bad_match

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("******************* suptest_hello_world: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.