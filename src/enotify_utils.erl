-module(enotify_utils).

-export([find_executable/1]).

find_executable(Cmd) ->
    case code:priv_dir(enotify) of
        {error, bad_name} ->
            error_logger:info_msg("Executable ~s not found", [Cmd]);
        Priv ->
            Path = filename:join(Priv, Cmd),
            case filelib:is_regular(Path) of
                true  ->
                    Path;
                false ->
                    error_logger:info_msg("Executable ~s not found", [Cmd])
            end
    end.
