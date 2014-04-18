%%
%% Demonstrates an ill-behaved supervisor.  The restart intensity for this
%% supervisor is such that any call to SupTestsHelloWorld:crash_hello() will
%% cause reached_max_restart_intensity to be reached causing this supervisor
%% to exit along with all remaining gen_server processes (i.e., FriendlyFireVictim).
%%
-module(crash_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    lager:emergency("******************* crash_sup: START_LINK~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%%     HelloWorldServer = ?CHILD(suptest_hello_world, worker),
    lager:emergency("******************* crash_sup: INIT~n", []),
	%%
	%% This sleep is put here to ensure that all children of non_std_sup have
	%% been started and are fully initialized prior to the children of this
	%% supervision tree being started.
	%%
	timer:sleep(1000),
	HelloWorldServer = {suptest_hello_world, {suptest_hello_world, start_link, []},
						permanent, 5000, worker, [suptest_hello_world]},
	SupTestsCallsHelloWorld = {suptest_calls_hello_world, {suptest_calls_hello_world, start_link, []},
						permanent, 5000, worker, [suptest_calls_hello_world]},
	FriendlyFireVictim = {friendly_fire_victim, {friendly_fire_victim, start_link, []},
						permanent, 5000, worker, [friendly_fire_victim]},
    {ok, { {one_for_one, 1, 1}, [HelloWorldServer, SupTestsCallsHelloWorld, 
								 FriendlyFireVictim] } }.

