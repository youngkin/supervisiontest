%%
%% non_std_sup demonstrates a supervisor who's children don't manage their own
%% state.  This is contrary to the Erlang-way in that when a supervisor restarts
%% failed children they should be able to recover their state (except for certain
%% dynamic state that can't be recomputed across restarts - see the concept of
%% an error kernel for more information).
%%
%% non_std_startup is a gen_server that starts all the other children of
%% non_std_sup.  After starting those children it sets the state of one
%% of them to mimic what's done in pe_startup.  This tests that we can
%% migrate from this current approach to state management to a more
%% Erlang-y approach where gen_servers maintain their own state.
%%
%% Finally, the restart strategy is set to all-for-one because all children's
%% state is managed by non_std_startup gen_server.  So unless they're all 
%% restarted (including non_std_startup), failed children won't have the proper
%% state.
%%

-module(non_std_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    lager:emergency("******************* non_std_sup: start_link~n", []),
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Key, Value) ->
    lager:emergency("******************* non_std_sup: start_child(~p,~p)~n", [Key, Value]),
	supervisor:start_child(?SERVER, [Key, Value]).

start_child(Spec) ->
    lager:emergency("******************* non_std_sup: start_child(~p)~n", [Spec]),
	supervisor:start_child(?SERVER, Spec).  
  
init([]) ->
    lager:emergency("******************* non_std_sup: INIT~n", []),
	
	Child1 = {non_std_startup, {non_std_startup, start_link, []},
						permanent, 5000, supervisor, [non_std_startup]},
    {ok, { {one_for_all, 1, 1}, [Child1] } }.
  
  
  
  
