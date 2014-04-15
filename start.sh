# -init_debug & suptest_app
# erl -init_debug -K true +P 10000 -env ERL_MAX_PORTS 10000 -pa ebin -boot start_sasl -name suptest@rich.suptest.com -setcookie COOKIE -s application start suptest_app
#erl -K true +P 10000 -env ERL_MAX_PORTS 10000 -pa ebin -boot start_sasl -name suptest@rich.suptest.com -setcookie COOKIE -s application start suptest
# erl -K true +P 10000 -env ERL_MAX_PORTS 10000 -pa ebin -pa deps/lager/ebin -boot start_sasl -name suptest@rich.suptest.com -setcookie COOKIE -eval "application:start(compiler), application:start(syntax_tools), application:start(lager), application:start(suptest)." 
erl -K true +P 10000 -env ERL_MAX_PORTS 10000 -pa ebin -pa deps/lager/ebin -boot start_sasl -name suptest@rich.suptest.com -setcookie COOKIE -eval "application:start(suptest)." 