-module(helper_functions).
-export([randomize_list/1,randomize_list2/1]).
-export([float_list_to_binary/1]).


randomize_list(List) ->
    randomize_list(List, []).
randomize_list([], Output_list) ->
    Output_list;
randomize_list(Input_list, Output_list) ->
    {List1, List2} = lists:split(random:uniform(length(Input_list))-1, Input_list),
    io:format("Length of inputlist: ~w~n", [length(Input_list)]),
    case {length(List1) == 1, List2} of 
        {false, []} -> 
            New_input_list = List1,
            New_output_list = Output_list;
        {true, []} ->
            New_input_list = [],
            New_output_list = [hd(List1)|Output_list];
        {_, _} -> 
            New_input_list = List1 ++ tl(List2),
            New_output_list = [hd(List2)|Output_list]
            
    end,
    randomize_list(New_input_list, New_output_list).

randomize2_helper(A) ->
    [{_, B}] = A,
    B.

randomize_list2(List) ->
    Input_table = ets:new(temp_table, []),
    lists:foreach(fun(Element) ->
        ets:insert(Input_table, {ets:info(Input_table, size), Element})
    end, List),
    List_length = ets:info(Input_table, size),
    lists:foreach(fun(Index) -> 
        J = random:uniform(List_length-1),
        [{_,A}] = ets:lookup(Input_table, Index),
        [{_,B}] = ets:lookup(Input_table, J),
        ets:insert(Input_table, { J, A }),
        ets:insert(Input_table, { Index, B })
    end, lists:seq(0, ets:info(Input_table, size)-1)),
    Output_list  = [ randomize2_helper(ets:lookup(Input_table, Index))|| Index <- lists:seq(0, ets:info(Input_table, size)-1)],
    ets:delete(Input_table),
    Output_list.

float_list_to_binary(List)
    when is_list(List) ->
        float_list_to_binary(List, <<>>).
float_list_to_binary([], Binary) 
    when is_binary(Binary) ->
        Binary;
float_list_to_binary([Head| Rest], Binary)
    when is_list(Rest), is_binary(Binary), is_float(Head) ->
        float_list_to_binary(Rest, <<Head:64/float, Binary/binary>>).
