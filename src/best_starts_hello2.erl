%% @author uyounri
%% @doc Demonstrates handling initialization of a process's state in the init/1
%%		function and in a helper function that could potentially be exported
%%		and called by another process.  The "state" in this module is a set of
%%		gen_servers which it starts and is linked to.  This module makes these
%%		gen_servers available to it's clients.
%%


-module(best_starts_hello2).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, ping/0, crash/0, get_hello_servers/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

ping() ->
    gen_server:call(?MODULE, ping).

crash() ->
    gen_server:call(?MODULE, crash).

%%
%% Returns a list of the best_hello2 PIDs managed by this gen_server.  The PIDs
%% can be used by clients to request services from those PIDs.  This function
%% could be optimized to provide scheduling/load-balancing capability for its
%% managed processes.
%%
get_hello_servers() ->
	gen_server:call(?MODULE, get_hello_servers).
	
start_link() ->
	io:format("******************* best_starts_hello2: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Callbacks
%% ====================================================================

init([]) ->
	io:format("******************* best_starts_hello2: INIT~n", []),
	%%
	%% Must be an async invocation or a deadlock will result.
	%%
	%% Either of PID ! MESSAGE or gen_server_cast() methods will work.
	%%
%% 	self() ! post_initialization,
	gen_server:cast(self(), post_initialization),
    {ok, []}.

handle_call(ping, _From, State) ->
    io:format("******************* best_starts_hello2: PING~n", []),
    {reply, {ok, pong}, State};

handle_call(crash, _From, State) ->
    io:format("******************* best_starts_hello2: CRASH!!!~n", []),
	X=1,
    {reply, X=2, State};

handle_call(get_hello_servers, _From, State) ->
    io:format("******************* best_starts_hello2: get_hello_servers!!!~n", []),
	{reply, State, State};

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
	io:format("******************* best_starts_hello2: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Helpers
%% ====================================================================

initialize() ->
	{ok, Pid1} = gen_server:start_link(best_hello2, [], []),
	{ok, Pid2} = gen_server:start_link(best_hello2, [], []),
 	{ok, Pid3} = gen_server:start_link(best_hello2, [], []),
	State = [Pid1, Pid2, Pid3],
	State.
	
