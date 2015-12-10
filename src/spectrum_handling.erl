-module(spectrum_handling).

-export([read_spectrum_from/2]).
-export([provide_spectra/2]).

% @doc Read and return a spectrum from a given source
read_spectrum_from(filesystem, Path) ->
    case file:consult(Path) of
        {ok, Spectrum} ->
            hd(Spectrum);
        {error, Reason} ->
            {error, Reason}
    end;
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
