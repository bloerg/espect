-module(benchmark).
-export([speed_test_get_neuron_spectrum_distance/2]).
-export([speed_test_update_neuron/2]).
-export([speed_test_event_handler_trigger_neuron_compare/1, speed_test_event_handler_trigger_neuron_update/1]).
-export([speed_test_neighbourhood_function1/5, speed_test_neighbourhood_function2/5, speed_test_neighbourhood_function3/5, speed_test_neighbourhood_function4/5]).


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
 
speed_test_neighbourhood_function1(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates, Repetitions) ->
    T1 = os:timestamp(),
    lists:foreach(
        fun(_X) ->
            neuron:neighbourhood_function1(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates)
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", neighbourhood_functions per second: ", Repetitions/timer:now_diff(T2,T1)*1000000})
.

speed_test_neighbourhood_function2(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates, Repetitions) ->
    Sigma_begin = 1.0,
    Sigma_end = 0.0625,
    Alpha_begin = 0.25,
    Alpha_end = 0.01,
    Two_times_sigma_squared = 2 * math:pow(neuron:sigma(Iteration, Max_iteration, Sigma_begin, Sigma_end), 2),
    Alpha = neuron:alpha(Iteration, Max_iteration, Alpha_begin, Alpha_end),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_X) ->
            neuron:neighbourhood_function2(Two_times_sigma_squared, Alpha, Neuron_coordinates, BMU_coordinates)
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", neighbourhood_functions per second: ", Repetitions/timer:now_diff(T2,T1)*1000000})
.

%fastest, until now
speed_test_neighbourhood_function3(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates, Repetitions) ->
    Sigma_begin = 1.0,
    Sigma_end = 0.0625,
    Alpha_begin = 0.25,
    Alpha_end = 0.01,
    Two_times_sigma_squared = 2 * math:pow(neuron:sigma(Iteration, Max_iteration, Sigma_begin, Sigma_end), 2),
    Alpha = neuron:alpha(Iteration, Max_iteration, Alpha_begin, Alpha_end),
    Neuron_BMU_coordinate_distance = vector_operations:vector_distance(Neuron_coordinates, BMU_coordinates),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_X) ->
            neuron:neighbourhood_function3(Two_times_sigma_squared, Alpha, Neuron_BMU_coordinate_distance)
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", neighbourhood_functions per second: ", Repetitions/timer:now_diff(T2,T1)*1000000})
.


speed_test_neighbourhood_function4(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates, Repetitions) ->
    Sigma_begin = 1.0,
    Sigma_end = 0.0625,
    Alpha_begin = 0.25,
    Alpha_end = 0.01,
    Two_times_sigma_squared = 2 * math:pow(neuron:sigma(Iteration, Max_iteration, Sigma_begin, Sigma_end), 2),
    Alpha = neuron:alpha(Iteration, Max_iteration, Alpha_begin, Alpha_end),
    Neuron_BMU_coordinate_distance = vector_operations:vector_distance(Neuron_coordinates, BMU_coordinates),
    Exp_one_over_two_times_sigma_squared = math:exp(-1 / Two_times_sigma_squared),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_X) ->
            neuron:neighbourhood_function4(Exp_one_over_two_times_sigma_squared, Alpha, Neuron_BMU_coordinate_distance)
        end,
        [X || X <- lists:seq(1,Repetitions)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", neighbourhood_functions per second: ", Repetitions/timer:now_diff(T2,T1)*1000000})
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
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", compares per second: ", Iterations*Number_of_neurons/timer:now_diff(T2,T1)*1000000}).
    
speed_test_event_handler_trigger_neuron_update(Iterations) ->
    Number_of_neurons = length(gen_event:which_handlers(neuron_event_manager)),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_Iteration) ->
            neuron_event_handler:trigger_neuron_update({update_async, spectrum_dispatcher:get_spectrum(spectrum_dispatcher), [3,4]})
        end,
        [Iteration || Iteration <- lists:seq(1, Iterations)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", updates per second: ", Iterations*Number_of_neurons/timer:now_diff(T2,T1)*1000000}).
