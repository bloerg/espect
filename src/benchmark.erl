-module(benchmark).
-export([speed_test_get_neuron_spectrum_distance/2]).
-export([speed_test_update_neuron/2]).
-export([speed_test_event_handler_trigger_neuron_compare/1, speed_test_event_handler_trigger_neuron_update/1]).
-export([speed_test_neighbourhood_function1/5, speed_test_neighbourhood_function2/5, speed_test_neighbourhood_function3/5, speed_test_neighbourhood_function4/5]).
-export([speed_test_vector_length/2, speed_test_vector_length2/2, speed_test_vector_length3/2, speed_test_vector_length4/2]).

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
    Number_of_neurons = length(gen_event:which_handlers({global, neuron_event_manager})),
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
    Number_of_neurons = length(gen_event:which_handlers({global, neuron_event_manager})),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_Iteration) ->
            neuron_event_handler:trigger_neuron_update({update_async, spectrum_dispatcher:get_spectrum(spectrum_dispatcher), [3,4]})
        end,
        [Iteration || Iteration <- lists:seq(1, Iterations)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", updates per second: ", Iterations*Number_of_neurons/timer:now_diff(T2,T1)*1000000}).


speed_test_vector_length(Iterations, Vector_length) ->
    Vector = sample_spectra:make_sine_spectrum(Vector_length),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_Iteration) ->
            vector_operations:vector_length(Vector)
        end,
        [Iteration || Iteration <- lists:seq(1, Iterations)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", vector_lengths per second: ", Iterations/timer:now_diff(T2,T1)*1000000}).

speed_test_vector_length2(Iterations, Vector_length) ->
    Vector = sample_spectra:make_sine_spectrum(Vector_length),
    T1 = os:timestamp(),
    lists:foreach(
        fun(_Iteration) ->
            vector_operations:vector_length2(Vector)
        end,
        [Iteration || Iteration <- lists:seq(1, Iterations)]
    ),
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", vector_lengths per second: ", Iterations/timer:now_diff(T2,T1)*1000000}).


speed_test_vector_length3(Iterations, Vector_length) ->
    Vector = lists:seq(1000000,1000000+Vector_length),
    speed_test_vector_length3(Iterations, Iterations, Vector, os:timestamp()).
speed_test_vector_length3(Iterations, 0 , _Vector, T1) ->
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", vector_lengths per second: ", Iterations/timer:now_diff(T2,T1)*1000000}),
    ok;
speed_test_vector_length3(Iterations, Iteration, Vector, T1) ->
    vector_operations:vector_length(Vector),
    speed_test_vector_length3(Iterations, Iteration - 1, Vector, T1).

speed_test_vector_length4(Iterations, Vector_length) ->
    Vector = array:fix(array:from_list(lists:seq(1,Vector_length))),
    speed_test_vector_length4(Iterations, Iterations, Vector, os:timestamp()).
speed_test_vector_length4(Iterations, 0 , _Vector, T1) ->
    T2 = os:timestamp(),
    erlang:display({debug, "time in microseconds: ", timer:now_diff(T2, T1), ", vector_lengths per second: ", Iterations/timer:now_diff(T2,T1)*1000000}),
    ok;
speed_test_vector_length4(Iterations, Iteration, Vector, T1) ->
    vector_operations:vector_length({array, Vector}),
    speed_test_vector_length4(Iterations, Iteration - 1, Vector, T1).
