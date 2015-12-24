-module(iteration_state_server).
-beaviour(gen_server).

-export([start/2, start_link/2, start/3, start_link/3]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_call/3]).
-export([init/1]).
-export([get_iteration/1, next_iteration/0]).


%%Start an iteration state server

start(Server_name, Iteration, Max_iteration) ->
    gen_server:start(Server_name, ?MODULE, [Iteration, Max_iteration], []). 

start_link(Server_name, Iteration, Max_iteration) ->
    gen_server:start_link(Server_name, ?MODULE, [Iteration, Max_iteration], []).

start(Iteration, Max_iteration) ->
    gen_server:start(?MODULE, [Iteration, Max_iteration], []).

start_link(Iteration, Max_iteration) ->
    gen_server:start_link(?MODULE, [Iteration, Max_iteration], []).


stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .

init(State) ->
    {ok, State}.

get_iteration(Server_name) ->
    gen_server:call(Server_name, get_iteration).

next_iteration() ->
    gen_server:call(?MODULE, next_iteration).

handle_call(get_iteration, _From, [Iteration, Max_iteration]) ->
    {reply, Iteration, [Iteration, Max_iteration]};

handle_call(next_iteration, _From, [Iteration, Max_iteration]) ->
    io:format("Doing nothing but increasing a variable ~w~n", [{Iteration, Iteration+1}]),
    io:format("...also, taking the time ~w~n", [os:timestamp()]),
    {reply, Iteration + 1, [Iteration + 1, Max_iteration]}.

