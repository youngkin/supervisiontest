%%
%% Demonstrates that gen_servers under a different supervisor are indeed
%% independent from the failing gen_servers under suptest_sup.  Failures in those 
%% gen_servers will cause crash_sup to exit but won't have an effect on 
%% gen_servers supervised here.  Note that the restart strategy is the same
%% as crash_sup, {one_for_one, 1, 1} where the "1,1" isn't very robust (which
%% is appropriate in some cases).
%%

-module(better_sup).

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
    lager:emergency("******************* suptest_better_sup: START_LINK~n", []),
%% 	lager:info("!!!!!!!!!!!!!!!!!!!! Test Lager install !!!!!!!!!!", []),
%% 	error_log:info("!!!!!!!!!!!!!!!!!!!! Test Lager install !!!!!!!!!!", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    lager:emergency("******************* suptest_better_sup: INIT~n", []),
	%%
	%% This sleep is put here to ensure that all children of non_std_sup have
	%% been started and are fully initialized prior to the children of this
	%% supervision tree being started.
	%%
	timer:sleep(1000),
	ProtectedServer = {protected_server, {protected_server, start_link, []},
						permanent, 5000, worker, [protected_server]},
    {ok, { {one_for_one, 1, 1}, [ProtectedServer] } }.

