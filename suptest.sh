#
# Start system under test 
#
#/opt/local/bin/erl -noshell -K true +P 10000 -env ERL_MAX_PORTS 10000 -pa ebin -pa deps/lager/ebin -boot start_sasl -name suptest@richsmac.ecollege-dev.com -setcookie COOKIE -eval "application:start(compiler), application:start(syntax_tools), application:start(lager), application:start(suptest)." >> /dev/null &

/opt/local/bin/erl -noshell -K true +P 10000 -env ERL_MAX_PORTS 10000 -pa ebin -pa deps/lager/ebin -pa deps/goldrush/ebin -boot start_sasl -name suptest@richsmac.ecollege-dev.com -setcookie COOKIE -config app -eval "application:start(suptest)." >> /dev/null &

#
# Run test
#
/opt/local/bin/erl -B -name escript@richsmac.ecollege-dev.com -setcookie COOKIE -s escript -- suptest.escript