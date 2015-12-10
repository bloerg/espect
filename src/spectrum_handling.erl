-module(spectrum_handling).

-export([read_spectrum_from/2]).
-export([provide_spectra/3]).

% @doc Read and return a spectrum from a given source
read_spectrum_from(filesystem, Path) ->
    {ok, Spectrum} = file:consult(Path),
    hd(Spectrum);
read_spectrum_from(random_sine, Spectrum_length) ->
    sample_spectra:make_sine_spectrum(Spectrum_length)
.

% @get a number of spectra from a given source
provide_spectra(filesystem, Directory, Number_of_spectra) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            [read_spectrum_from(filesystem, string:concat(Directory, File)) || File <- Files];
        {error, Reason} ->
            {error, Reason}
    end.
