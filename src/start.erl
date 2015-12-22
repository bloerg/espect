-module(start).
-export([start/0]).

start() ->

    %~ %spectrum dispatcher
    %~ erlang:display("Trying to start spectrum dispatcher..."),
    %~ case spectrum_dispatcher:start({local, spectrum_dispatcher},{filesystem, "/var/tmp/sine/", binary, 1}, 0, 200) of
        %~ {error,{already_started,Pid}} -> 
            %~ erlang:display("Spectrum dispatcher already running. Stopping and restarting..."),
            %~ exit(Pid, shutdown),
            %~ timer:sleep(1000),
            %~ {ok, _} = spectrum_dispatcher:start({local, spectrum_dispatcher},{filesystem, "/var/tmp/sine/", binary, 1}, 0, 200);
        %~ {ok, Pid} ->
            %~ erlang:display("Started Spectrum dispatcher with pid"),
            %~ erlang:display(Pid)
    %~ end,
    
    % neuron supervisor
    erlang:display("Trying to start supervision tree with worker neurons"),
    case neuron_supervisor:start_link({local, testsup}, 10, {child_specs, 0, 200, {neuron_coordinates, 0}}) of
        {error,{already_started,Pid}} ->
            erlang:display("Neuron supervision tree already running. Stopping and restarting..."),
            exit(Pid, shutdown),
            timer:sleep(5000),
            {ok, _} = neuron_supervisor:start_link({local, testsup}, 10, {child_specs, 0, 200, {neuron_coordinates, 100, 100, 0}});
        {ok, Pid} ->
            erlang:display("Started supervision tree with pid"),
            erlang:display(Pid),
            case learning_step_manager:next_learning_step() of
                ok -> erlang:display("Reset spectrum_dispatcher");
                _Other -> erlang:display("could not reset spectrum dispatcher")
            end,
            
            ok
    end.

