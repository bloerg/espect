-module(benchmark).
-export([speed_test_get_neuron_spectrum_distance/2]).
-export([speed_test_update_neuron/2]).
-export([speed_test_event_handler_trigger_neuron_compare/1]).



speed_test_get_neuron_spectrum_distance(Spectrum_width, Repetitions) ->
    T1 = os:timestamp(),
    Neuron = sample_spectra:make_sine_spectrum(Spectrum_width),
    Spectrum = sample_spectra:make_sine_spectrum(Spectrum_width),
    neuron:start({local, benchmark_test_neuron}, [1,2], Neuron, [], 0, 200),
    lists:foreach(
        fun(_Iteration) ->
            neuron:get_neuron_spectrum_distance(benchmark_test_neuron, Spectrum, [1,2])
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", compares per second: ", Repetitions/timer:now_diff(T2,T1)*1000000}),
    neuron:stop(benchmark_test_neuron)

.
 
speed_test_update_neuron(Spectrum_width, Repetitions) ->
    T1 = os:timestamp(),
    Neuron = sample_spectra:make_sine_spectrum(Spectrum_width),
    Spectrum = sample_spectra:make_sine_spectrum(Spectrum_width),
    neuron:start({local, benchmark_test_neuron}, [1,2], Neuron, [], 0, 200),
    lists:foreach(
        fun(_Iteration) ->
            neuron:update_neuron(benchmark_test_neuron, Spectrum, [2,3])
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", updates per second: ", Repetitions/timer:now_diff(T2,T1)*1000000}),
    neuron:stop(benchmark_test_neuron)

.

speed_test_event_handler_trigger_neuron_compare(Iterations) ->
    Number_of_neurons = length(gen_event:which_handlers(neuron_event_manager)),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_Iteration) ->
            neuron_event_handler:trigger_neuron_compare({compare, spectrum_dispatcher:get_spectrum(spectrum_dispatcher), [1,2,3]})
        end,
        [Iteration || Iteration <- lists:seq(1, Iterations)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", updates per second: ", Iterations*Number_of_neurons/timer:now_diff(T2,T1)*1000000}).
    
