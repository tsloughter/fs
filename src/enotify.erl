-module(enotify).

-export([subscribe/0
        ,subscribe/2
        ,find_executable/1]).

-define(EVENTS, [created, closed, modified, moved, deleted, undefined]).

subscribe() ->
    subscribe("", ?EVENTS).

subscribe(Path, Events) ->
    fs_server:start_link(filename:absname(Path), Events).

find_executable(Cmd) ->
    case code:priv_dir(?MODULE) of
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
