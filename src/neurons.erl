-module(neurons).
-beaviour(gen_server).

-export([start/5, start_link/5, start/6, start_link/6]).
-export([stop/0, stop/1, terminate/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([init/1, load_spectra_to_neurons_worker/1]).
-export([add_neurons/2, remove_neurons/2]).
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
    neuron_coordinate_range = [], %interval of the sequence of neuron coordinates, element of [0 ... Max_x * Max_y]; this is for initalisation, it may be wrong after adding and remove spectra using add_spectra() and remove_spectra()
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
    gen_event:delete_handler({global, neuron_event_manager}, {neuron_event_handler, {neuron, self()}}, [{pid, self()}]),
    gen_event:delete_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),
    ok.



init(State) ->
    gen_event:add_handler({global, neuron_event_manager}, {neuron_event_handler, {neuron, self()}}, [{pid, self()}]),
    gen_event:add_handler({global, iteration_event_manager}, {iteration_event_handler, self()}, [{pid, self()}, {module, ?MODULE}]),

    [First_neuron, Last_neuron] = State#neuron_worker_state.neuron_coordinate_range,
    [X_max, _Y_max] = State#neuron_worker_state.som_dimensions,
    
    {ok, [
            [ 
            %~ #neuron {
                    %~ neuron_vector = binary_to_term(spectrum_dispatcher:get_spectrum(State#neuron_worker_state.spectrum_dispatcher)),
                    
                    %~ %% get Spectrum from spectrum_dispatcher
                    %~ neuron_vector = spectrum_dispatcher:get_spectrum_for_neuron_initialization(State#neuron_worker_state.spectrum_dispatcher),
                    
                    %~ %% get Spectrum path form spectrum_dispatcher and load spectrum from filesystem
                    %~ neuron_vector = load_spectrum_from_filesystem(
                        %~ spectrum_dispatcher:get_spectrum_path_for_neuron_initialization(State#neuron_worker_state.spectrum_dispatcher)
                    %~ ),
                    %~ neuron_coordinates = neuron_supervisor:get_x_y_from_sequence(X_max, Sequence_number)
                %~ } || Sequence_number <- lists:seq(First_neuron, Last_neuron) 
            ],
            State
        ]
    }.


load_spectrum_from_filesystem({Direction, Path}) ->
    case Direction of 
        forward ->
            [_Spectrum_id, Spectrum] = spectrum_handling:read_spectrum_from(
                filesystem, 
                Path,
                binary
            );
        %% if all spectra are dispatched but neurons keep asking,
        %% return random spectras from the list of same spectra, but reversed
        reverse ->
            [_Spectrum_id, Temp_spectrum] = spectrum_handling:read_spectrum_from(
                filesystem, 
                Path,
                binary
            ),
            Spectrum = lists:reverse(Temp_spectrum)
    end,
    Spectrum.

%% @doc init neuron_workers with spectra
load_spectra_to_neurons_worker(Server_name) ->
    gen_server:cast(Server_name, load_spectra_to_neurons_worker).

%% @doc add neurons to the neurons list
%List_of_neurons must be list of #neuron record
add_neurons(Server_name, List_of_neurons) ->
    gen_server:call(Server_name, {add_neurons, List_of_neurons}).

%% @doc deletes neurons from the neurons list and returns the deleted neurons
remove_neurons(Server_name, Number) ->
    gen_server:call(Server_name, {remove_neurons, Number}).

%% @doc computes the vector distance between neuron and spectrum and sends the result to the caller
get_neuron_spectrum_distance(Server_name, Spectrum_with_id) ->
    gen_server:call(Server_name, {compare, Spectrum_with_id}).
    
%% @doc async vector distance computing
get_neuron_spectrum_distance({async, BMU_manager}, Server_name, Spectrum_with_id) ->
    gen_server:cast(Server_name, {{async, BMU_manager}, {compare, Spectrum_with_id}}).

set_bmu(Neurons_worker_name, Neuron_coordinates, Spectrum_id) ->
    gen_server:call(Neurons_worker_name, {set_bmu, Neuron_coordinates, Spectrum_id}).

%% @doc calls the function to compute the new neuron vector
update_neuron(Server_name, BMU_neuron_coordinates) ->
    gen_server:call(Server_name, {update_neuron, BMU_neuron_coordinates}).

update_neuron(async, Server_name, BMU_neuron_coordinates) ->
    gen_server:cast(Server_name, {update_neuron, BMU_neuron_coordinates}).

set_iteration(Server_name, New_iteration) ->
    gen_server:call(Server_name, {set_iteration, New_iteration}).

handle_cast(
    {{async, BMU_manager}, {compare, Spectrum_with_id}}, 
    [Neurons, Neuron_worker_state]) ->
        %~ erlang:display({"comparing spectrum: ", Spectrum_with_id}),
        [Spectrum_metadata, Spectrum] = Spectrum_with_id,
        %% FIXME: I want this with tail recursion, not map
        NewNeurons =
            lists:map(fun(Neuron) ->
                Neuron_vector_difference = vector_operations:vector_difference(Neuron#neuron.neuron_vector, Spectrum),
                Neuron#neuron{
                    last_spectrum_neuron_vector_difference = Neuron_vector_difference
                }
            end,
            Neurons
            ),
        [Min_spectrum_neuron_distance, Min_spectrum_neuron_distance_coordinates] = 
            lists:foldl(fun(Neuron, [Min_distance, Min_distance_neuron_coordinates]) ->
                Spectrum_vector_distance = vector_operations:vector_length(Neuron#neuron.last_spectrum_neuron_vector_difference),
                % We don't want to consider a neuron twice. So we match against the bmu. If the neuron already is a BMU, skip it and just return the already known minimum value.
                case [Spectrum_vector_distance < Min_distance, binary_to_term(Neuron#neuron.bmu_to_spectrum_id)] of
                    [false, [-1, -1, -1]] -> [Min_distance, Min_distance_neuron_coordinates]; 
                    [true,  [-1, -1, -1]] -> [Spectrum_vector_distance, Neuron#neuron.neuron_coordinates];
                    _Other_list -> [Min_distance, Min_distance_neuron_coordinates]
                end
            end,
            [576460752303423487, []], NewNeurons),  %A large Number, every distance should be less than this number, taken from http://www.erlang.org/doc/efficiency_guide/advanced.html

        %~ erlang:display({"sending bmu candidate to bmu_manager: ", [Min_spectrum_neuron_distance_coordinates, 
                 %~ Spectrum_metadata, 
                 %~ Min_spectrum_neuron_distance
            %~ ]}), 
        bmu_manager:neuron_spectrum_distance(BMU_manager,
            [Min_spectrum_neuron_distance_coordinates, 
                 Spectrum_metadata, 
                 Min_spectrum_neuron_distance
            ]
        ),
        {noreply, [NewNeurons, Neuron_worker_state]};

handle_cast({update_neuron, BMU_neuron_coordinates}, [Neurons, Neuron_worker_state]) ->
    %~ io:format("Updating neurons with: ~w~n", [{bmu_coordinates, BMU_neuron_coordinates, neuron_coordinate_range, Neuron_worker_state#neuron_worker_state.neuron_coordinate_range}]),
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
    learning_step_manager:update_complete(self()),
    {noreply, [NewNeurons, Neuron_worker_state]};

handle_cast(load_spectra_to_neurons_worker, [_Neurons, Neuron_worker_state]) ->
    [First_neuron, Last_neuron] = Neuron_worker_state#neuron_worker_state.neuron_coordinate_range,
    [X_max, _Y_max] = Neuron_worker_state#neuron_worker_state.som_dimensions,
    {noreply, [
            [ 
            #neuron {
                    %~ neuron_vector = binary_to_term(spectrum_dispatcher:get_spectrum(State#neuron_worker_state.spectrum_dispatcher)),
                    
                    %~ %% get Spectrum from spectrum_dispatcher
                    neuron_vector = spectrum_dispatcher:get_spectrum_for_neuron_initialization(Neuron_worker_state#neuron_worker_state.spectrum_dispatcher),
                    
                    %% get Spectrum path form spectrum_dispatcher and load spectrum from filesystem
                    %~ neuron_vector = load_spectrum_from_filesystem(
                        %~ spectrum_dispatcher:get_spectrum_path_for_neuron_initialization(Neuron_worker_state#neuron_worker_state.spectrum_dispatcher)
                    %~ ),
                    neuron_coordinates = neuron_supervisor:get_x_y_from_sequence(X_max, Sequence_number)
                } || Sequence_number <- lists:seq(First_neuron, Last_neuron) 
            ],
            Neuron_worker_state
        ]
    };

handle_cast(stop, Neuron_state) ->
    {stop, normal, Neuron_state}.

handle_call({add_neuron, List_of_neurons}, _From, [Neurons, Neuron_worker_state]) ->
    {reply, ok, [Neurons ++ List_of_neurons, Neuron_worker_state]};

handle_call({remove_neuron, Number}, _From, [Neurons, Neuron_worker_state]) ->
    {Reply, New_neurons} = lists:split(min(length(Neurons), Number), Neurons),
    {reply, Reply, [New_neurons, Neuron_worker_state]};

handle_call(
    {set_bmu, BMU_neuron_coordinates, BMU_spectrum_id}, _From, [Neurons, Neuron_worker_state]) ->
        %~ io:format("Setting BMU: ~w~n", [{coordinates, BMU_neuron_coordinates, spectrum_id, BMU_spectrum_id}]),
        {reply, ok, [
            lists:map(fun(Neuron) ->
                    case Neuron#neuron.neuron_coordinates of
                        BMU_neuron_coordinates -> Neuron#neuron{bmu_to_spectrum_id = term_to_binary(BMU_spectrum_id) };
                        _Other -> Neuron
                    end
                end,
            Neurons
            ), 
            Neuron_worker_state
        ]};
        
handle_call({set_iteration, New_iteration}, _From, [Neurons, Neuron_worker_state]) ->
    {reply, ok, [Neurons, Neuron_worker_state#neuron_worker_state{iteration = New_iteration}]}.


handle_info(Message, State) ->
    io:format("Unexpected message: ~w~n",[{Message, State}]).

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

