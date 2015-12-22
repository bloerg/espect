-module(neurons).
-beaviour(gen_server).

-export([start/5, start_link/5, start/6, start_link/6]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
%~ -export([handle_call/3]).
-export([init/1]).
-export([get_neuron_spectrum_distance/2, get_neuron_spectrum_distance/3]).
-export([set_bmu/3, set_iteration/2]).
-export([update_neuron/2, update_neuron/3]).

%for testing, remove later
-export([alpha/4, sigma/4, neighbourhood_function1/4, neighbourhood_function2/4, neighbourhood_function3/3, neighbourhood_function4/3]).

-record(neuron, {
    neuron_vector = [], %the vector representing the spectrum occupied by the neuron
    neuron_coordinates = [], %the coordinates of the neuron in the som
    bmu_to_spectrum_id = term_to_binary([-1,-1,-1]), %the neuron is BMU to spectrum with id
    last_spectrum_neuron_vector_difference = [] %the vector difference computed in the compare process, used later for the update computation
}).

-record(neuron_worker_state, {
    spectrum_dispatcher = spectrum_dispatcher,  %Name of the spectrum dispatcher server
    neuron_coordinate_range = [], %interval of the sequence of neuron coordinates, element of [0 ... Max_x * Max_y]
    som_dimensions = [], % maximum x and maximum y coordinate of the self organizing map
    iteration = 0, %iteration step
    max_iteration = 200 %maximum number of iterations
}).

%%Start a neuron server
start(Server_name, Spectrum_dispatcher, Neuron_coordinate_range, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start(Server_name, ?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_coordinate_range = Neuron_coordinate_range,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

start_link(Server_name, Spectrum_dispatcher, Neuron_coordinate_range, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start_link(Server_name, ?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_coordinate_range = Neuron_coordinate_range,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

start(Spectrum_dispatcher, Neuron_coordinate_range, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start(?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_coordinate_range = Neuron_coordinate_range,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

start_link(Spectrum_dispatcher, Neuron_coordinate_range, [Max_x, Max_y], Iteration, Max_iteration) ->
    gen_server:start_link(?MODULE, 
            #neuron_worker_state {
                spectrum_dispatcher = Spectrum_dispatcher,
                neuron_coordinate_range = Neuron_coordinate_range,
                som_dimensions = [Max_x, Max_y],
                iteration = Iteration,
                max_iteration = Max_iteration
            }
        ,[]
    ).

%%Stop a neuron server
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Server_name) ->
    gen_server:cast(Server_name, stop).
terminate(_Reason, _Neuron_state) ->
    gen_event:delete_handler(neuron_event_manager, {neuron_event_handler, self()}, []),
    ok.



init(State) ->
    gen_event:add_handler(neuron_event_manager, {neuron_event_handler, {neuron, self()}}, [{pid, self()}]),
    gen_event:add_handler(iteration_event_manager, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),

    [First_neuron, Last_neuron] = State#neuron_worker_state.neuron_coordinate_range,
    [X_max, _Y_max] = State#neuron_worker_state.som_dimensions,
    
    {ok, [
            [ #neuron {
                    %~ neuron_vector = binary_to_term(spectrum_dispatcher:get_spectrum(State#neuron_worker_state.spectrum_dispatcher)),
                    neuron_vector = spectrum_dispatcher:get_spectrum_for_neuron_initialization(State#neuron_worker_state.spectrum_dispatcher),
                    neuron_coordinates = neuron_supervisor:get_x_y_from_sequence(X_max, Sequence_number)
                } || Sequence_number <- lists:seq(First_neuron, Last_neuron) 
            ],
            State
        ]
    }.

%% @doc computes the vector distance between neuron and spectrum and sends the result to the caller
get_neuron_spectrum_distance(Server_name, Spectrum_with_id) ->
    gen_server:call(Server_name, {compare, Spectrum_with_id}).
    
%% @doc async vector distance computing
get_neuron_spectrum_distance({async, Result_receiver_name}, Server_name, Spectrum_with_id) ->
    gen_server:cast(Server_name, {{async, Result_receiver_name}, {compare, Spectrum_with_id}}).

set_bmu(Neurons_worker_name, Neuron_coordinates, Spectrum_id) ->
    gen_server:cast(Neurons_worker_name, {set_bmu, Neuron_coordinates, Spectrum_id}).

%% @doc calls the function to compute the new neuron vector
update_neuron(Server_name, BMU_neuron_coordinates) ->
    gen_server:call(Server_name, {update_neuron, BMU_neuron_coordinates}).

update_neuron(async, Server_name, BMU_neuron_coordinates) ->
    gen_server:cast(Server_name, {update_neuron, BMU_neuron_coordinates}).

set_iteration(Server_name, New_iteration) ->
    gen_server:cast(Server_name, {set_iteration, New_iteration}).

handle_cast(
    {{async, Result_receiver_name}, {compare, Spectrum_with_id}}, 
    [Neurons, Neuron_worker_state]) ->
        erlang:display({"comparing spectrum: ", Spectrum_with_id}),
        [Spectrum_metadata, Spectrum] = Spectrum_with_id,
        %% FIXME: I want this with tail recursion, not map
        NewNeurons =
            lists:map(fun(Neuron) ->
                case Neuron#neuron.neuron_vector of 
                    [] -> Neuron;
                    _Other ->
                        Neuron_vector_difference = vector_operations:vector_difference(Neuron#neuron.neuron_vector, Spectrum),
                        Neuron#neuron{
                            last_spectrum_neuron_vector_difference = Neuron_vector_difference
                        }
                end
            end,
            Neurons
            ),
        [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates] = 
            lists:foldl(fun(Neuron, [Min_distance, Min_distance_neuron_coordinates]) ->
                case Neuron#neuron.last_spectrum_neuron_vector_difference of 
                    [] -> Spectrum_vector_distance = 576460752303423487;
                    _Other_vector -> Spectrum_vector_distance = vector_operations:vector_length(Neuron#neuron.last_spectrum_neuron_vector_difference)
                end,
                % We don't want to consider a neuron twice. So we match against the bmu. If the neuron already is a BMU, skip it and just return the already known minimum value.
                case [Spectrum_vector_distance < Min_distance, binary_to_term(Neuron#neuron.bmu_to_spectrum_id)] of
                    [false, [-1, -1, -1]] -> [Min_distance, Min_distance_neuron_coordinates]; 
                    [true,  [-1, -1, -1]] -> [Spectrum_vector_distance, Neuron#neuron.neuron_coordinates];
                    _Other_list -> [Min_distance, Min_distance_neuron_coordinates]
                end
            end,
            [576460752303423487, []], NewNeurons),  %A large Number, every distance should be less than this number, taken from http://www.erlang.org/doc/efficiency_guide/advanced.html

        erlang:display({"sending bmu candidate to bmu_manager: ", [Min_spectrum_neuron_distance_coordinates, 
                 Spectrum_metadata, 
                 Min_spectrum_neuron_distance
            ]}), 
        bmu_manager:neuron_spectrum_distance(Result_receiver_name,
            [Min_spectrum_neuron_distance_coordinates, 
                 Spectrum_metadata, 
                 Min_spectrum_neuron_distance
            ]
        ),
        {noreply, [NewNeurons, Neuron_worker_state]};

handle_cast({update_neuron, BMU_neuron_coordinates}, [Neurons, Neuron_worker_state]) ->
    NewNeurons =
        %% FIXME: I want this with tail recursion, not map
        lists:map(fun(Neuron) ->
                case length(Neuron#neuron.neuron_vector) of 
                    0 -> Neuron;
                    _Other ->
                        Neuron#neuron{
                            neuron_vector = vector_operations:vector_sum(
                                Neuron#neuron.neuron_vector,
                                vector_operations:scalar_multiplication(
                                    neighbourhood_function1(
                                        Neuron_worker_state#neuron_worker_state.iteration, 
                                        Neuron_worker_state#neuron_worker_state.max_iteration, 
                                        Neuron#neuron.neuron_coordinates, 
                                        BMU_neuron_coordinates
                                    ),
                                    %vector_operations:vector_difference(BMU_spectrum, Neuron_vector)
                                    Neuron#neuron.last_spectrum_neuron_vector_difference
                                )
                            )
                        }
                end
        end,
        Neurons
        ),
    {noreply, [NewNeurons, Neuron_worker_state]};

handle_cast(
    {set_bmu, BMU_neuron_coordinates, BMU_spectrum_id}, [Neurons, Neuron_worker_state]) ->
        erlang:display({"setbmu", BMU_neuron_coordinates, BMU_spectrum_id}),
        {noreply, [
            lists:map(fun(Neuron) ->
                    case Neuron#neuron.neuron_coordinates of
                        BMU_neuron_coordinates -> Neuron#neuron{bmu_to_spectrum_id = term_to_binary(BMU_spectrum_id) };
                        _Other -> Neuron
                    end
                end,
            Neurons
            ), 
            Neuron_worker_state
        ]}.

%~ handle_cast({update_neuron, BMU_spectrum, BMU_coordinates}, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
    %~ {noreply, [Neuron_coordinates, 
             %~ vector_operations:vector_sum(Neuron_vector,
                %~ vector_operations:scalar_multiplication(
                    %~ neighbourhood_function1(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates),
                    %~ vector_operations:vector_difference(BMU_spectrum, Neuron_vector)
                %~ )
             %~ ),
             %~ BMU,
             %~ Iteration,                
             %~ Max_iteration
            %~ ]
    %~ };
%~ handle_cast({set_iteration, New_iteration}, [Neuron_coordinates, Neuron_vector, BMU, _Old_iteration, Max_iteration]) ->
    %~ {noreply, [Neuron_coordinates, Neuron_vector, BMU, New_iteration, Max_iteration]};

%~ handle_cast(stop, Neuron_state) ->
    %~ {stop, normal, Neuron_state}.

%~ handle_call(
    %~ {compare, Spectrum, Spectrum_metadata}, _From, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
        %~ Reply = [Neuron_coordinates, 
                 %~ Spectrum_metadata, 
                 %~ vector_operations:vector_distance(Neuron_vector, Spectrum)
                %~ ],
        %~ {reply, Reply, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]}
    %~ ;
%~ handle_call(
    %~ {set_bmu, New_BMU}, _From, [Neuron_coordinates, Neuron_vector, _Old_BMU, Iteration, Max_iteration]) ->
        %~ {reply, ok, [Neuron_coordinates, Neuron_vector, New_BMU, Iteration, Max_iteration]}
    %~ ;

%~ handle_call(
    %~ {update_neuron, BMU_spectrum, BMU_coordinates}, _From, [Neuron_coordinates, Neuron_vector, BMU, Iteration, Max_iteration]) ->
        %~ %% The if is for benchmark. Iteration should not go back to 0, actually
        %~ if
            %~ Iteration < Max_iteration -> Iteration2 = Iteration + 1;
            %~ Iteration == Max_iteration -> Iteration2 = 0
        %~ end,
        %~ {reply, ok, [Neuron_coordinates, 
                 %~ vector_operations:vector_sum(Neuron_vector,
                    %~ vector_operations:scalar_multiplication(
                        %~ neighbourhood_function1(Iteration, Max_iteration, Neuron_coordinates, BMU_coordinates),
                        %~ vector_operations:vector_difference(BMU_spectrum, Neuron_vector)
                    %~ )
                 %~ ),
                 %~ BMU,
                 %~ Iteration2,                
                 %~ Max_iteration
                %~ ]
        %~ }
%~ .



%% BMU-Update related functions
alpha(T, T_max, Alpha_begin, Alpha_end) ->
    Alpha_begin * math:pow(Alpha_end / Alpha_begin, T/T_max).

sigma(T, T_max, Sigma_begin, Sigma_end) ->
    Sigma_begin * math:pow(Sigma_end / Sigma_begin, T/T_max).

neighbourhood_function1(T, T_max, Neuron_coordinates, BMU_coordinates) ->
    Sigma_begin = 1.0,
    Sigma_end = 0.0625,
    Alpha_begin = 0.25,
    Alpha_end = 0.01,
    alpha(T, T_max, Alpha_begin, Alpha_end) * 
        math:exp(
            -vector_operations:vector_distance(BMU_coordinates, Neuron_coordinates) 
            / (2 * math:pow(
                            sigma(T, T_max, Sigma_begin, Sigma_end),
                            2
                            )
            )
        )
.
neighbourhood_function2(Two_times_sigma_squared, Alpha, Neuron_coordinates, BMU_coordinates) ->
        Alpha * math:exp(
            -vector_operations:vector_distance(BMU_coordinates, Neuron_coordinates) 
            / Two_times_sigma_squared
        )
.
neighbourhood_function3(Two_times_sigma_squared, Alpha, Neuron_BMU_coordinate_distance) ->
        Alpha * math:exp(
            -Neuron_BMU_coordinate_distance / Two_times_sigma_squared
        )
.

neighbourhood_function4(Exp_one_over_two_times_sigma_squared, Alpha, Neuron_BMU_coordinate_distance) ->
        Alpha*math:pow(Exp_one_over_two_times_sigma_squared, Neuron_BMU_coordinate_distance).

