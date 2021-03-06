-module(sample_spectra).
-export([make_sine_spectrum/1, make_binary_spectrum/1]).
-export([make_sample_spectrum_bucket/4, make_sample_spectrum_bucket/5]).

%% @author Jörg Brünecke <dev@bloerg.de>
%% @doc sample spectrum module
%% The module provides operations to generate random sine spectra.
%% The Spectra are lists of given length containing values in the codomain corresponding to the domain [0, 2pi]
%% The Spectra can be generated in bulk and written to files (one spectrum each) in a provided directory for later use.


make_sample_spectrum_bucket([MaxX, MaxY], BaseDirectory, SpectrumLength, single_binary_file)
    when 
        is_integer(MaxX),
        is_integer(MaxY),
        is_integer(SpectrumLength),
        is_list(BaseDirectory),
        MaxX == MaxY,
        SpectrumLength > 0
    ->
        Output_file=string:join([BaseDirectory, "/all_specs.bin"], ""),
        lists:foreach(fun([X,Y]) ->
            Data = term_to_binary([[Y*MaxX + X, X+Y, X], make_sine_spectrum(SpectrumLength)]),
            Output_binary = <<Data/binary,$\n>>,
            file:write_file(Output_file, Output_binary, [append])
        end,
        [ [X,Y]
              || X <- lists:seq(0,MaxX), Y <- lists:seq(0, MaxY)
            ]
        )
    .
        

make_sample_spectrum_bucket([MaxX, MaxY], BaseDirectory, SpectrumLength, Spectrum_generator_function, File_format) 
    when 
        is_integer(MaxX),
        is_integer(MaxY),
        is_integer(SpectrumLength),
        SpectrumLength > 0
    ->
        lists:foreach(
            fun([X,Y]) -> 
                FileName=string:join(["spec-", integer_to_list(Y*MaxX+X), "-", integer_to_list(X+Y), "-", integer_to_list(X), ".spec"], ""),
                %erlang:display({debug, sample_spectra, [X,Y], X*(Y+1)+X}),
                %erlang:display({debug, sample_spectra, write_to_file, OutputFile}),
                case File_format of
                    plain ->
                        Output_file_plain=string:join([BaseDirectory, "/", FileName], ""),
                        ok = write_sample_spectrum_to_plain_file(
                            Output_file_plain, 
                            Spectrum_generator_function(SpectrumLength)
                        );
                    binary ->
                        Output_file_binary=string:join([BaseDirectory, "/binary/", FileName], ""),
                        Spectrum = Spectrum_generator_function(SpectrumLength), %make_sine_spectrum(SpectrumLength),
                        ok = write_sample_spectrum_to_binary_file(
                            Output_file_binary, 
                            term_to_binary([[Y*MaxX + X, X+Y, X], Spectrum])
                        ),
                        %%write plain version of spectrum to plain directory for easy plotting
                        Output_file_plain=string:join([BaseDirectory, "/plain/", FileName], ""),
                        ok = write_sample_spectrum_to_plain_file(
                            Output_file_plain, 
                            Spectrum
                        )
                end
            end,
            [[X,Y] || X <- lists:seq(0,MaxX), Y <- lists:seq(0, MaxY)]
        )
    .

write_sample_spectrum_to_plain_file(Filename, Spectrum) ->
    file:write_file(Filename, io_lib:fwrite("~p.\n", [Spectrum])).

write_sample_spectrum_to_binary_file(Filename, Spectrum) ->
    file:write_file(Filename, [Spectrum]).

make_binary_spectrum(Number_of_elements) ->
    [ random:uniform(2) -1 || _I <- lists:seq(1, Number_of_elements)].

make_sine_spectrum(NumberOfElements) ->
    A=random:uniform()*10,
    B=random:uniform()*3,
    C=random:uniform()*2*math:pi(),
    make_sine_spectrum(NumberOfElements, (B*2*math:pi()-C)/NumberOfElements, [A,B,C], []).

make_sine_spectrum(Counter, XStep, [A,B,C], Spectrum) when Counter > 0 ->
    X = 2*math:pi() - Counter * XStep,
    make_sine_spectrum(Counter -1, XStep, [A,B,C], [A*math:sin(B*X-C)|Spectrum]);
make_sine_spectrum(0, _, _, Spectrum) ->
    Spectrum.
