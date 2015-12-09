-module(benchmark).
-export([speed_test_compare_neuron_with_spectrum/2]).


speed_test_compare_neuron_with_spectrum(Spectrum_width, Repetitions) ->
    T1 = os:timestamp(),
    Neuron = sample_spectra:make_sine_spectrum(Spectrum_width),
    Spectrum = sample_spectra:make_sine_spectrum(Spectrum_width),
    neuron:start({local, benchmark_test_neuron}, [1,2], Neuron, [], 0),
    lists:foreach(
        fun(_Iteration) ->
            neuron:compare_neuron_with_spectrum(test, Spectrum, [1,2])
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time: ", timer:now_diff(T2, T1), ", ratio: ", Repetitions/timer:now_diff(T2,T1)*1000000})
.
 
