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
			timer, sleep, [5000])
		 ]),

	io:format("~n************* CONFIRM sunny day *************~n"),

	io:format("rpc:call(non_std_child1:get_state()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child1, get_state, [])
		 ]),
	io:format("rpc:call(non_std_child2:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child2, ping, [])
		 ]),
	io:format("rpc:call(non_std_child3:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child3, ping, [])
		 ]),

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

	%% Wait a bit so we don't exceed the application's restart intensity
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [1500])
		 ]),

	io:format("rpc:call(protected_server:crash()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			protected_server, crash, [])
		 ]),
	%% Wait until the process is restarted
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [100])
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

	%% Wait a bit so we don't exceed the application's restart intensity
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [1500])
		 ]),

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
%% CRASH THE WORLD test
%%
	io:format("~n************** CRASH THE WORLD!!!! ***************~n"),
	io:format("***** Even best_friendly_fire_victim is unreachable *****~n"),

	BestSupPidBefore = rpc:call('suptest@richsmac.ecollege-dev.com',
			erlang, whereis, [best_sup]),
	NonStdSupPidBefore = rpc:call('suptest@richsmac.ecollege-dev.com',
			erlang, whereis, [non_std_sup]),
	
	%% Wait a bit so we don't exceed the application's restart intensity
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [1500])
		 ]),

    %% Call crash() twice to exceed restart intensity which will cause the entire
	%% application, i.e., all supervision trees, to terminate.
	io:format("rpc:call(non_std_child1:crash()) on suptest_app: ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child1, crash, [])
		 ]),
	io:format("rpc:call(best_friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_friendly_fire_victim, ping, [])
		 ]),
	io:format("rpc:call(non_std_child1:crash()) on suptest_app: ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child1, crash, [])
		 ]),
	io:format("rpc:call(best_friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_friendly_fire_victim, ping, [])
		 ]),
	io:format("rpc:call(non_std_child1:crash()) on suptest_app: ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child1, crash, [])
		 ]),
	io:format("rpc:call(best_friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_friendly_fire_victim, ping, [])
		 ]),

	io:format("***** Wait for all gen_servers to be restarted and accept requests*****~n"),
	
	%% Wait a bit so we don't exceed the application's restart intensity
	io:format("rpc:call(timer:sleep()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			timer, sleep, [1500])
		 ]),

	BestSupPidAfter = rpc:call('suptest@richsmac.ecollege-dev.com',
			erlang, whereis, [best_sup]),
	NonStdSupPidAfter = rpc:call('suptest@richsmac.ecollege-dev.com',
			erlang, whereis, [non_std_sup]),

	%% Checking the supervisors's PIDs verifies that they were restarted as a
	%% result of the crashes provoked above.
	io:format("BestSupPidBefore ~p; BestSupPidAfter ~p.  They should be different~n", [BestSupPidBefore, BestSupPidAfter]),
	io:format("NonStdSupPidBefore ~p; NonStdSupPidAfter ~p.  They should be different~n", [NonStdSupPidBefore, NonStdSupPidAfter]),
	
	io:format("rpc:call(best_friendly_fire_victim:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			best_friendly_fire_victim, ping, [])
		 ]),
	io:format("rpc:call(non_std_child2:ping()): ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			non_std_child2, ping, [])
		 ]),
%%
%% CLEANUP & STOP test
%%
	io:format("~n************** STOP ***************~n"),
	
	io:format("rpc:call(init:stop()) on suptest VM: ~p~n", 
		[rpc:call('suptest@richsmac.ecollege-dev.com',
			init, stop, [])
		 ]).
		 
