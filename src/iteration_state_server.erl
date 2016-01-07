-module(iteration_state_server).
-beaviour(gen_server).

-export([start/2, start_link/2, start/3, start_link/3]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_call/3]).
-export([init/1]).
-export([get_iteration/0, next_iteration/0]).


%%Start an iteration state server

start(Server_name, Iteration, Max_iteration) ->
    gen_server:start(Server_name, ?MODULE, [Iteration, Max_iteration, os:timestamp()], []). 

start_link(Server_name, Iteration, Max_iteration) ->
    gen_server:start_link(Server_name, ?MODULE, [Iteration, Max_iteration, os:timestamp()], []).

start(Iteration, Max_iteration) ->
    gen_server:start(?MODULE, [Iteration, Max_iteration, os:timestamp()], []).

start_link(Iteration, Max_iteration) ->
    gen_server:start_link(?MODULE, [Iteration, Max_iteration, os:timestamp()], []).


stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).

terminate(_Reason, _Neuron_state) ->
    ok
    .

init(State) ->
    {ok, State}.

get_iteration() ->
    gen_server:call({global, ?MODULE}, get_iteration).

next_iteration() ->
    gen_server:call({global, ?MODULE}, next_iteration, 60*1000).

handle_call(get_iteration, _From, [Iteration, Max_iteration, Learning_step_begin_timestamp]) ->
    {reply, Iteration, [Iteration, Max_iteration, Learning_step_begin_timestamp]};

handle_call(next_iteration, _From, [Iteration, Max_iteration, Learning_step_begin_timestamp]) ->
    case Iteration of 
        Max_iteration ->
            io:format("I am done.");
        _Smaller_than_max_iteration -> 
            io:format("Increasing Iteration ~p~n", [{Iteration, "->", Iteration+1}]),
            io:format("Last iteration step was running for ~w seconds.~n", [timer:now_diff(os:timestamp(), Learning_step_begin_timestamp) / 1000000]),
            io:format("Writing som state to disk~n", []),
            som_manager:dump_som(string:join(["/var/tmp/som_", integer_to_list(Iteration + 1), ".csv"],"")),
            iteration_event_handler:trigger_iteration_update(Iteration + 1),
            learning_step_manager:set_iteration({global, learning_step_manager}, Iteration +1),
            neuron_event_handler:trigger_neuron_compare({compare, spectrum_dispatcher:get_spectrum_with_id({global, spectrum_dispatcher})})
    end,
    {reply, Iteration + 1, [Iteration + 1, Max_iteration, os:timestamp()]}.

