-module(spectrum_handling).

-export([read_spectrum_from/2, read_spectrum_from/3]).
-export([provide_spectra/2, provide_spectra/3]).
-export([provide_spectra_list/2]).

% @doc Read and return a spectrum from a given source
read_spectrum_from(filesystem, Path, plain) ->
    case file:consult(Path) of
        {ok, Spectrum} ->
            hd(Spectrum);
        {error, Reason} ->
            {error, Reason}
    end;
read_spectrum_from(filesystem, Path, binary) ->
    case file:read_file(Path) of
        {ok, Spectrum} ->
            binary_to_term(Spectrum);
        {error, Reason} ->
            {error, Reason}
    end.
read_spectrum_from(random_sine, Spectrum_length) ->
    sample_spectra:make_sine_spectrum(Spectrum_length)
.



% @get a number of spectra from a given source
provide_spectra(filesystem, Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            [read_spectrum_from(filesystem, string:concat(Directory, File)) || File <- Files];
        {error, Reason} ->
            {error, Reason}
    end.

provide_spectra(random_sine, Spectrum_length, Number_of_spectra) ->
    [sample_spectra:make_sine_spectrum(Spectrum_length) || _X <- lists:seq(1, Number_of_spectra)].



% @get a list of files in a directory
provide_spectra_list(filesystem, Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            Files;
        {error, Reason} ->
            {error, Reason}
    end.
