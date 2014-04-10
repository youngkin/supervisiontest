%% @author uyounri
%% @doc Demonstrates a gen_server that can access, and crash, another gen_server
%%		started and managed from another process. 
%%
%%      Given that the called gen_server is managed by another process a method
%%		is needed that guarantees that the called gen_server is available before
%%		this gen_server accepts requests.  This could be addessed by putting
%% 		a post-initialization hook into this module that could sleep() for a
%%		period of time sufficient to let the system stabilize before accepting
%%		messages.  This approach is built into this module.
%%


-module(best_calls_hello2).

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
	io:format("******************* best_calls_hello2: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("******************* best_calls_hello2: INIT~n", []),
	%%
	%% Must be an async invocation or a deadlock will result.
	%%
	%% Either of PID ! MESSAGE or gen_server_cast() methods will work.
	%%
	%% In this case, the post-initialization prevents this gen_server from being
	%% called by an external client for some arbitrary delay period.
%% 	self() ! post_initialization,
	gen_server:cast(self(), post_initialization),
    {ok, []}.

%% ====================================================================
%% Callbacks
%% ====================================================================

handle_call(hello, _From, State) ->
    io:format("******************* best_calls_hello2: HELLO~n", []),
	[BestHelloServer | _] = best_starts_hello2:get_hello_servers(),
	Result = gen_server:call(BestHelloServer, hello),
    {reply, {ok, Result}, State};

handle_call(crash_hello, _From, State) ->
    io:format("******************* best_calls_hello2: CRASH!!!~n", []),
	[BestHelloServer | _] = best_starts_hello2:get_hello_servers(),
	Result = gen_server:call(BestHelloServer, crash_hello),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(post_initialization, _State) ->
	%%
	%% Demonstrates that gen_server initialization can be completed AFTER
	%% init/1 has finished.  This approach may be useful when a relatively
	%% complex initialization sequence is required.
	%%
	NewState = initialize(),
	{noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(post_initialization, _State) ->
	%%
	%% Demonstrates that gen_server initialization can be completed AFTER
	%% init/1 has finished.  This approach may be useful when a relatively
	%% complex initialization sequence is required.
	%%
	NewState = initialize(),
	{noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("******************* best_calls_hello2: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Helpers
%% ====================================================================

initialize() ->
	timer:sleep(1000).
