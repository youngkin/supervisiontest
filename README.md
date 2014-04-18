SupervisionTest
===============

An Erlang demonstration of different supervision strategies along with different 
initialization strategies and failure scenarios.  It also demonstrates the use
of the Basho Lager logging framework.

Details
==============

This project is used to demonstrate various characteristics of supervision trees
in Erlang.  It has 3 supervision sub-trees that exhibit different restart semantics
and contain children that demonstrate "interesting" behaviors.

**suptest_sup** is the root supervisor for the suptest application.  It has a
reasonable restart strategy that results in any failed children (i.e., the 
supervisors listed below) being restarted when/if they fail.  In addition, it
enforces a startup sequencing and dependency ordering between its child
supervisors by using a rest_for_one restart stratgegy.

**non_std_sup**, is a supervisor with children that don't follow the normal Erlang
approach to state management by initializing their state within the init/1
function.  Because of how startup and state management of the children is
handled, a all_for_one restart strategy is used.
	- **non_std_startup** is the only gen_server started directly from 	
	  non_std_sup's init/1 function.  Its role is to start the remaining children
	  and in the case of non_std_child1, setting its state.  Normally gen_servers
	  establish their state in their init/1 functions but in this case this is
	  handled by non_std_startup.
	- **non_std_child-n**, where "n" is 1, 2, or 3, are a set of gen_servers that
	  are started from non_std_startup.  non_std_child1 has its state established
	  by non_std_startup after it completes its startup processing.  All 3 
	  gen_servers support "ping" and "crash" functions.

**crash_sup**, as the name suggests, implements an unforgiving restart strategy that
causes the entire supervision tree, including the itself, to terminate and
restart when a "crash" request is made on a child gen_server.  Its children also
support basic success behaviors like "ping" and "say_hello".  Its children are
   - **suptest_hello_world**: is a simple gen_server that supports a hello() 
   	 function to verify that it's running and a crash() function that can force
   	 the gen_server to crash.
   - **suptest_calls_hello_world**: represents a client gen_server that delegates
     calls to suptest_hello_world.  It demonstrates that crashes and exceptions
     in the called gen_server propagate to the calling gen_server.  It doesn't
     handle these conditions so it will crash if the called function throws an
     exception or otherwise causes the called gen_server to exit.
   - **friendly_fire_victim**: demonstrates that an otherwise innocent gen_server
     will be the victim of sibling crashes that exceed the maximum restart
     intentisty defined by the supervisor.

**better_sup** is a little better only in the sense that it demonstrates that it, and
its child, will continue to operate even if crash_sup and it's children are 
restarted.  Its child, **protected_server**, also provides a "crash" function which,
when called, will cause it and better_sup to be terminated and restarted.

**best_sup** implements a robust restart strategy.  It will restart failed children
across a reasonable number of failures in a given time period. Its children 
demonstrate the following:
   - **best_friendly_fire_victim**: perhaps poorly named, it demonstrates resilience
     in the face of failures of its siblings.
   - **best_starts_hello2**: starts a set of child gen_servers (**best_hello2**) 
     and links to those children.  It hands out PIDs referencing those children 
     so its clients can call those children without requiring registered server names.  
     Being linked to those children guarantees that it will terminate if any of 
     it's children terminate.  Its children represent state that must be 
     recreated across restarts (i.e., they must be restarted and their PIDs
     maintained as state).
   - **best_calls_hello2**: is a client of best_starts_hello2 (to get a best_hello2
   	 reference) and a client of best_hello2.  It also has a startup dependency
   	 on best_hello2 as best_hello2 must be available to call. It demonstrates one
   	 way to maintain this dependency relationship despite possibly being started
   	 prior to best_hello2, especially across restarts which seem to be less
   	 predictable WRT to supervisor managed start sequencing.
   - **best_hello2*: is a simple gen_server that supports a hello() 
     function to verify that it's running and a crash() function that can force
     the gen_server to crash.  It's essentially a re-implementation of 
     suptest_hello_world.

The "suptest.sh" file executes the application in an erlang shell and then runs
the test script, "suptest.escript", to test/illuminate various aspects of supervision
tree behavior under various restart-strategy/failure scenarios.

This app should be built using rebar (e.g., ./rebar clean compile).

It comes with 2 supporting scripts:
   - suptest.sh: starts an erlang shell that runs the suptest application and then
     runs suptest.escript.
   - suptest.escript: is an erlang script that exercises the suptest application
     started by start.sh.  It verifies suptest starts successfully and that it
     handles failures in the expected manner.
     
Enhancements:
   - Re-implement the tests supported by start.sh and suptest.escript in eUnit
     and/or CommonTest.
   - Get Lager working.
