#!/usr/bin/env escript
main(_) ->
%%
%% START & CONFIRM sunny day scenarios
%%
	io:format("~n************* START *************~n"),
	io:format("~n************* Verify remote node available *************~n"),

	io:format("net_adm:ping(): ~p~n", 
		[net_adm:ping('suptest@richsmac.ecollege-dev.com')]),	

	%% Wait until system stabilizes
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [1000])
		 ]),

	io:format("~n************* CONFIRM sunny day *************~n"),

	io:format("rpc:call(suptest_calls_hello_world:say_hello()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			suptest_calls_hello_world, say_hello, [])
		 ]),
	io:format("rpc:call(friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			friendly_fire_victim, ping, [])
		 ]),

	io:format("rpc:call(protected_server:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			protected_server, ping, [])
		 ]),

	io:format("rpc:call(best_friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_friendly_fire_victim, ping, [])
		 ]),
	io:format("rpc:call(best_starts_hello2:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_starts_hello2, ping, [])
		 ]),
	io:format("rpc:call(best_calls_hello2:say_hello()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_calls_hello2, say_hello, [])
		 ]),
		 


%%
%% CRASH & CONFIRM crash and crash recovery of crash_sup and children
%%
	io:format("~n************* CRASH suptest_sup and recovery*************~n"),

	io:format("rpc:call(suptest_calls_hello_world:crash_hello()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			suptest_calls_hello_world, crash_hello, [])
		 ]),

	io:format("~n************* CONFIRM friendly_fire_victim DEAD  *************~n"),
	io:format("************* (This sometimes works due to timing)  *************~n"),

	io:format("rpc:call(friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			friendly_fire_victim, ping, [])
		 ]),

	%% Wait for restart
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [1000])
		 ]),

	io:format("~n************* CONFIRM friendly_fire_victim RESTARTED *************~n"),

	io:format("rpc:call(friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com', 
			friendly_fire_victim, ping, [])
		 ]),
		 
%%
%% CRASH & CONFIRM protected_server restarts
%%		 
	io:format("~n************* CRASH protected_server *************~n"),

	io:format("rpc:call(protected_server:crash()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			protected_server, crash, [])
		 ]),
	io:format("~n************* protected_server on better_sup RESTARTED *************~n"),

	io:format("rpc:call(protected_server:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			protected_server, ping, [])
		 ]),

%%
%% CRASH & CONFIRM best_sup & children start and are operational	
%%	 
	io:format("~n************* CRASH & CONFIRM best_sup and children RESTARTED *************~n"),

	io:format("rpc:call(best_calls_hello2:crash_hello()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_calls_hello2, crash_hello, [])
		 ]),

	%% Wait for best_calls_hello2 to finish initializing (it's sleeps immediately
	%% after being restarted) restart
%	io:format("rpc:call(timer:sleep()): ~p~n", 
%		[rpc:call('suptest@richsmac.ecollege-dev.com',
%			timer, sleep, [1000])
%		 ]),

	io:format("rpc:call(best_calls_hello2:say_hello()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_calls_hello2, say_hello, [])
		 ]),

	io:format("rpc:call(best_friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_friendly_fire_victim, ping, [])
		 ]),

	io:format("rpc:call(best_starts_hello2:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_starts_hello2, ping, [])
		 ]),
%%
%% CLEANUP & STOP test
%%
	io:format("~n************** STOP ***************~n"),
	
	io:format("rpc:call(init:stop()) on suptest_app: ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			init, stop, [])
		 ]).
		 
