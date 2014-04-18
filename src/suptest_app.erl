-module(suptest_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	lager:start(),
	lager:emergency("******************* suptest_app STARTING!!!!", []),
    suptest_sup:start_link().

stop(_State) ->
	lager:emergency("******************* suptest_app STOPPING!!!!", []),
    ok.

