-module(enotify_inotifywait_win32).

-export([start_port/1
        ,line_to_event/2
        ,line_parser/0]).

start_port(Path) ->
    Path1 = filename:absname(Path),
    Args = ["-m", "-r", Path1],
    erlang:open_port({spawn_executable, enotify_utils:find_executable("inotifywait.exe")},
                     [stream, exit_status, {line, 16384}, {args, Args}]).

line_to_event(Line, RE) ->
    {match, [Dir, Flags1, DirEntry]} = re:run(Line, RE, [{capture, all_but_first, list}]),
    Flags = [convert_flag(F) || F <- string:tokens(Flags1, ",")],
    Path = filename:join(Dir,DirEntry),
    {Path, Flags}.

convert_flag("CREATE") -> created;
convert_flag("MODIFY") -> modified;
convert_flag("DELETE") -> deleted;
convert_flag("MOVE")   -> moved;
convert_flag(_)        -> undefined.

line_parser() ->
    {ok, R} = re:compile("^(.*\\\\.*) ([A-Z_,]+) (.*)$", [unicode]),
    R.
