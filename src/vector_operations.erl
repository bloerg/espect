-module(vector_operations).
-export([vector_distance_squared/2, vector_distance/2, vector_difference/2, vector_difference/3, vector_sum/2, vector_length/1, vector_length2/1, scalar_multiplication/2, reverse_vector/1]).
-export([vector_generate/2]).
-export([vector_length_squared/1]).


%% @author Jörg Brünecke <dev@bloerg.de>
%% @doc Basic vector operations module
%% The module provides operations on simple n-dimensional cartesian vectors.


vector_distance_squared(Vector1, Vector2) -> 
    vector_distance_squared(Vector1, Vector2, 0).
vector_distance_squared([], [], Result) ->
    Result;
vector_distance_squared([First_v1|Rest_v1], [First_v2|Rest_v2], Intermediate_result) ->
    C = First_v1 - First_v2,
    vector_distance_squared(Rest_v1, Rest_v2, Intermediate_result + (C*C)).


vector_distance(Vector1, Vector2) when
    length(Vector1) == length(Vector2) -> 
        math:sqrt(vector_distance_squared(Vector1, Vector2)).    
    

vector_difference(Vector1, Vector2) when
    length(Vector1) == length(Vector2) ->
        vector_difference(Vector1, Vector2, []).
vector_difference([],[], Result) ->
    reverse_vector(Result);
vector_difference([First_v1|Rest_v1], [First_v2|Rest_v2], Intermediate_result) ->
    vector_difference(Rest_v1, Rest_v2, [First_v1 - First_v2|Intermediate_result]).

vector_sum(Vector1, Vector2) when
    length(Vector1) == length(Vector2) ->
        vector_sum(Vector1, Vector2, []).
vector_sum([],[], Result) ->
    reverse_vector(Result);
vector_sum([First_v1|Rest_v1], [First_v2|Rest_v2], Intermediate_result) ->
    vector_sum(Rest_v1, Rest_v2, [First_v1 + First_v2|Intermediate_result]).


reverse_vector(Vector) ->
    reverse_vector(Vector, []).
reverse_vector([], Reversed_vector) ->
    Reversed_vector;
reverse_vector([Head|Tail], Out) ->
    reverse_vector(Tail, [Head |Out]).

scalar_multiplication(Scalar, Input_vector) ->
    scalar_multiplication(Scalar, Input_vector, []).
scalar_multiplication(_, [], Output_vector) ->
    reverse_vector(Output_vector);
scalar_multiplication(Scalar, [First_element|Rest], Output_vector) ->
    scalar_multiplication(Scalar, Rest, [First_element*Scalar|Output_vector]).


% with arrays, not so fast
vector_length_squared({array, Vector}) ->
    array:foldl(fun(_Index, Value, Length_squared) -> Length_squared + Value*Value end, 0, Vector);

%this is fast
vector_length_squared(Vector) ->
    vector_length_squared(Vector, 0).
vector_length_squared([Head| Tail], Sum_squared) ->
    vector_length_squared(Tail, Sum_squared + Head*Head);
vector_length_squared([], Sum_squared) ->
    Sum_squared.


vector_length({array, Vector}) ->
    math:sqrt(vector_length_squared({array, Vector}));
vector_length(Vector) ->
    math:sqrt(vector_length_squared(Vector)).





%this is slow
vector_length2(Vector) ->
    math:sqrt(lists:foldl(fun(Element, Length_squared) ->
        Length_squared + Element * Element
        end,
        0, Vector)
    ).



vector_generate(random, Length) when is_integer(Length), Length > 0 ->
    vector_generate_random(Length, []).
    
vector_generate_random(Length, Vector) when Length > 0 ->
    vector_generate_random(Length -1, [math:pow(-1,random:uniform(2)) * random:uniform(1000)|Vector]);
vector_generate_random(0,Vector) -> 
    Vector.
    
