%%
%% This is the root supervisor for the suptest application.  It starts 3 different
%% supervisors that exhibit 3 different kinds of restart strategies and, in the
%% case of best_sup, demostrate how state can be recovered across restarts
%% (after crashes).
%%

-module(suptest_sup).

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
    io:format("******************* suptest_sup: START_LINK~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%%     HelloWorldServer = ?CHILD(suptest_hello_world, worker),
    io:format("******************* suptest_sup: INIT~n", []),
	CrashSupervisor = {crash_sup, {crash_sup, start_link, []},
						permanent, 5000, supervisor, [crash_sup]},
	BetterSupervisor = {better_sup, {better_sup, start_link, []},
						permanent, 5000, supervisor, [better_sup]},
	BestSupervisor = {best_sup, {best_sup, start_link, []},
						permanent, 5000, supervisor, [best_sup]},
    {ok, { {one_for_one, 1, 1}, [CrashSupervisor, BetterSupervisor, 
								 BestSupervisor] } }.

