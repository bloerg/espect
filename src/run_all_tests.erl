-module(run_all_tests).
-export([start/0]).

start() ->
    eunit:test(vector_operations, [verbose])
    
    .
