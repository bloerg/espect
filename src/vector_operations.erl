-module(vector_operations).
-export([vector_distance_squared/2, vector_distance/2, vector_difference/2, vector_differencef/2, vector_sum/2, vector_length/1, vector_length2/1, scalar_multiplication/2, reverse_vector/1]).
-export([vector_generate/2]).
-export([vector_length_squared/1, vector_length_squaredf/1, vector_lengthf/1, vector_length_squaredI/1, vector_lengthI/1]).
-export([vector_lengthBf/1]).

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


vector_differencef(Vector1, Vector2) when
    length(Vector1) == length(Vector2), is_list(Vector1), is_list(Vector2) ->
        vector_differencef(Vector1, Vector2, []).
vector_differencef([],[], Result) 
    when is_list(Result) ->
    reverse_vector(Result);
vector_differencef([First_v1|Rest_v1], [First_v2|Rest_v2], Intermediate_result)
    when is_float(First_v1), is_float(First_v2), is_list(Rest_v1), is_list(Rest_v2), is_list(Intermediate_result)->
    vector_differencef(Rest_v1, Rest_v2, [First_v1 - First_v2|Intermediate_result]).

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
    
%this is fast
vector_length_squaredI(Vector) 
    when is_list(Vector) ->
    vector_length_squaredI(Vector, 0).
vector_length_squaredI([Head| Tail], Sum_squared)
    when is_integer(Sum_squared), is_integer(Head) ->
    vector_length_squaredI(Tail, Sum_squared + Head*Head);
vector_length_squaredI([], Sum_squared) 
    when is_integer(Sum_squared) ->
    Sum_squared.

%this is fast with floats and HIPE
vector_length_squaredf(Vector) 
    when is_list(Vector) ->
    vector_length_squaredf(Vector, 0.0).
vector_length_squaredf([Head| Tail], Sum_squared)
    when is_float(Head), is_float(Sum_squared), is_list(Tail)->
    vector_length_squaredf(Tail, Sum_squared + Head*Head);
vector_length_squaredf([], Sum_squared)
    when is_float(Sum_squared)->
    Sum_squared.

%vector length of a float binary
%assumes 64 Bit floats
vector_length_squaredBf(Vector)
    %~ when is_binary(Vector) ->
    %~ vector_length_squaredBf(Vector, 0.0).
    when is_binary(Vector) ->
    vector_length_squaredBf(Vector, byte_size(Vector) - 8, 0.0).

%~ vector_length_squaredBf(<<>>, Sum_squared) ->
    %~ Sum_squared;
%~ vector_length_squaredBf(<<Head:64/float,Rest/binary>>, Sum_squared)
    %~ when is_float(Head), is_binary(Rest), is_float(Sum_squared) ->
        %~ vector_length_squaredBf(Rest, Sum_squared + Head * Head).
vector_length_squaredBf(_Binary, -8, Sum_squared) ->
    Sum_squared;
vector_length_squaredBf(Vector, At, Sum_squared)
    when is_binary(Vector), is_integer(At), is_float(Sum_squared) ->
        <<_:At/binary, Val:64/float, _/binary>> = Vector,
        vector_length_squaredBf(Vector, At - 8, Sum_squared + Val*Val).


vector_length(Vector) ->
    math:sqrt(vector_length_squared(Vector)).

vector_lengthI(Vector)
    when is_list(Vector) ->
    trunc(math:sqrt(vector_length_squared(Vector))).

vector_lengthf(Vector)
    when is_list(Vector) ->
    math:sqrt(vector_length_squaredf(Vector)).

vector_lengthBf(Vector)
    when is_binary(Vector) ->
    math:sqrt(vector_length_squaredBf(Vector)).

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
    
