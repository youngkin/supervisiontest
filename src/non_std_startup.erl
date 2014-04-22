%% @author uyounri
%% @doc Demonstrates a gen_server that starts other children of its
%%		parent supervisor and then creates and sets their state (at least for
%%		non_std_child1).  Note, this is a very non standard way of managing
%% 		gen_server state.  Normally a gen_server like non_std_child1 would be
%% 		responsible for managing its own state removing the need for the complicated
%%		supervision tree and initialization process that is implemented in this
%%		class and its supervisor, non_std_sup.  That is the reason why the 
%%		modules in this supervision tree are all prefixed with "non_std".
%%


-module(non_std_startup).

-behavior(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, ping/0, crash/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

ping() ->
    gen_server:call(?MODULE, ping).

crash() ->
    gen_server:call(?MODULE, crash).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	lager:emergency("******************* non_std_startup: START_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:emergency("******************* non_std_startup: INIT~n", []),
	gen_server:cast(?MODULE, start_children),
    {ok, []}.


%%
%% Starts the app in 2 major phases, the primary and secondary phases.  The
%% primary phase starts gen_servers that have some post-startup configuration
%% requirements.
%%
%% The secondary phase includes gen_servers that can't be started until all
%% gen_servers in the primary phase have completed initialization.
%%
start() ->
  	start(get_startup_phases()),
	start_the_rest(get_secondary_startup_phases()).

%%
%% This function definition is for the primary startup phase.
%%
start([]) ->
  	ok;
start([Phase|Phases]) ->
  case start_phase(Phase) of
    ok ->
      start(Phases);
    ERROR ->
      ERROR
  end.
  
%%
%% This function definition is for the secondary startup phase
%%
start_the_rest([]) ->
  	ok;
start_the_rest([SecondaryPhase|SecondaryPhases]) ->
  case start_secondary_phase(SecondaryPhase) of
    ok ->
      start_the_rest(SecondaryPhases);
    ERROR ->
      ERROR
  end.
  
start_phase({Specs,Callback}) ->
	lists:foreach(
		fun(E) ->
			lager:emergency("******************* non_std_startup: Starting ~p...~n",[E]),
			case non_std_sup:start_child(E) of
				{ok, Pid} -> {ok, Pid};
				[Error] -> {error, Error}
			end
		end,
		Specs
	),
	call(Callback).

start_secondary_phase({Specs,Callback}) ->
	lists:foreach(
		fun(E) ->
			lager:emergency("******************* non_std_startup: Starting ~p...~n",[E]),
			case suptest_sup:start_child(E) of
				{ok, Pid} -> {ok, Pid};
				[Error] -> {error, Error}
			end
		end,
		Specs
	),
	call(Callback).

call(undefined) ->
  ok;
call(Callback) ->
  Callback().

get_startup_phases() ->
  Child1 = {
              non_std_child1,
              {non_std_child1,start_link, []},
              permanent,
              2000,
              worker,
              [non_std_child1]
           },

  Child2 = {
              non_std_child2,
              {non_std_child2,start_link, []},
              permanent,
              2000,
              worker,
              [non_std_child2]
           },
  Child3 = {
		non_std_child3,
		{non_std_child3, start_link, []},
		permanent,
		5000,
		worker,
		[non_std_child3]
	},
  
  [           
    {
      [Child1, Child2],
      fun() ->
        post_child1_started(),
        ok
      end
    },
    {
      [Child3],
      undefined
    }
  ].

get_secondary_startup_phases() ->
  CrashSup = {
              crash_sup,
              {crash_sup,start_link, []},
              permanent,
              2000,
              supervisor,
              [crash_sup]
           },

  BetterSup = {
              better_sup,
              {better_sup,start_link, []},
              permanent,
              2000,
              supervisor,
              [better_sup]
           },
  BestSup = {
			  best_sup,
              {best_sup,start_link, []},
              permanent,
              2000,
              supervisor,
              [best_sup]
	},
  
  [           
    {
      [CrashSup],
      undefined
    },
    {
      [BetterSup],
      undefined
    },
    {
      [BestSup],
      undefined
    }
  ].

post_child1_started() ->
	lager:emergency("******************* non_std_startup:post_child1_started~n",[]),
	non_std_child1:set_state("TheState").

%% ====================================================================
%% Callback functions (basically unused)
%% ====================================================================

handle_call(ping, _From, State) ->
    lager:emergency("******************* non_std_startup: PING~n", []),
    {reply, {ok, pong}, State};

handle_call(crash, _From, State) ->
    lager:emergency("******************* non_std_startup: CRASH!!!~n", []),
	X=1,
    {reply, X=2, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start_children, State) ->
    lager:emergency("******************* non_std_startup:start_children()~n", []),
	start(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	lager:emergency("******************* non_std_startup: TERMINATE~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
