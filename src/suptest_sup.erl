%%
%% This is the root supervisor for the suptest application.  It starts 3 different
%% supervisors that exhibit 3 different kinds of restart strategies and, in the
%% case of best_sup, demostrate how state can be recovered across restarts
%% (after crashes).
%%

-module(suptest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    lager:emergency("******************* suptest_sup: START_LINK~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Key, Value) ->
    lager:emergency("******************* suptest_sup: start_child(~p,~p)~n", [Key, Value]),
	supervisor:start_child(?MODULE, [Key, Value]).

start_child(Spec) ->
    lager:emergency("******************* suptest_sup: start_child(~p)~n", [Spec]),
	supervisor:start_child(?MODULE, Spec).  
  
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%%     HelloWorldServer = ?CHILD(suptest_hello_world, worker),
    lager:emergency("******************* suptest_sup: INIT~n", []),
	
	NonStdSupervisor = {non_std_sup, {non_std_sup, start_link, []},
						permanent, 5000, supervisor, [non_std_sup]},
%% 	CrashSupervisor = {crash_sup, {crash_sup, start_link, []},
%% 						permanent, 5000, supervisor, [crash_sup]},
%% 	BetterSupervisor = {better_sup, {better_sup, start_link, []},
%% 						permanent, 5000, supervisor, [better_sup]},
%% 	BestSupervisor = {best_sup, {best_sup, start_link, []},
%% 						permanent, 5000, supervisor, [best_sup]},
    {ok, { {rest_for_one, 1, 1}, [NonStdSupervisor] } }. %, CrashSupervisor, BetterSupervisor, 
								 %BestSupervisor] } }.

