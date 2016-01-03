-module(espect).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
    error_logger:logfile({open, "/var/tmp/espect.log"}),
    {ok, _Pid} = node_supervisor:start_link(),
    %~ neuron_supervisor:start_link({local, testsup}, {child_specs, 0, 200, {neuron_coordinates, 0}}),
    %~ neuron_event_handler:trigger_load_spectra_to_neurons_workers(),
    {ok, self()}.

stop(_State) ->
    ok.
