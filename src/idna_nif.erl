-module(idna_nif).

-export([init/0]).
-export([to_ascii_nif/1, to_unicode_nif/1]).


-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    NumScheds = erlang:system_info(schedulers),
    Arch = erlang:system_info(system_architecture),
    SoPath = filename:join([PrivDir, Arch, idna_nif]),
    erlang:load_nif(SoPath, NumScheds).


to_ascii_nif(_Label) ->
    exit(idna_not_loaded).

to_unicode_nif(_Label) ->
    exit(nif_not_loaded).
