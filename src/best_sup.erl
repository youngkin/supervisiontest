%%
%% Demonstrates:
%%		- That gen_servers under a different supervisor are indeed
%% 		  independent from the failing gen_servers under suptest_sup.  Failures 
%%		  in those gen_servers will cause the offending processes under the other
%%        supervisor to fail (and restart), but won't have an effect on 
%%		  gen_servers supervised here.
%%
%% 		- Start-up ordering can be specified by the supervisor (but start-up ordering)
%%		  of children started by this supervisor's children isn't necessarily
%%		  guaranteed.  See best_calls_hello2 for more details.
%%
%%		- Recovery of gen_server state across process restarts.
%%

-module(best_sup).

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
    io:format("******************* best_sup: START_LINK~n", []),
 	lager:info("!!!!!!!!!!!!!!!!!!!! Test Lager install !!!!!!!!!!", []),
 	lager:error("!!!!!!!!!!!!!!!!!!!! Test Lager ERROR message !!!!!!!!!!", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("******************* best_sup: INIT~n", []),
	
    BestStartsHello2 = ?CHILD(best_starts_hello2, worker),
    BestFriendlyFireVictim = ?CHILD(best_friendly_fire_victim, worker),
    BestCallsHello2 = ?CHILD(best_calls_hello2, worker),

	%% Max of 2 restarts in 5 seconds
	%%
	%% Startup order of the children is guaranteed by the order in which they
	%% are specified in the CHILD spec.
	%%
	%% On restart after crash, this order doesn't seems to be maintained, but
	%% the BestCallsHello2's restart is interspersed with BestStartsHello2's
	%% children which it needs in order to fulfill its responsibilities. This
	%% could be a problem.  Might consider a sleep() in best_calls_hello2:init/1
	%% to ensure startup ordering.  This can have a negative impact on start-up
	%% times though.
	%%
    {ok, { {one_for_one, 2, 5}, [BestStartsHello2, BestFriendlyFireVictim, BestCallsHello2] } }.

