-module(enotify_fsevents).

-export([start_port/1
        ,line_to_event/2
        ,line_parser/0]).

start_port(Path) ->
    erlang:open_port({spawn_executable, enotify_utils:find_executable("mac_listener")},
                     [stream, exit_status, {line, 16384}, {args, ["-F", Path]}]).

line_to_event(Line, _) ->
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    [_, Flags2] = string:tokens(Flags1, [$=]),
    {ok, T, _} = erl_scan:string(Flags2 ++ "."),
    {ok, Flags} = erl_parse:parse_term(T),
    {Path, Flags}.

line_parser() ->
    undefined.
