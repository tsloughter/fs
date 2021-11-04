-module(enotify_inotifywait).

-export([start_port/1
        ,line_to_event/2
        ,line_parser/0]).

start_port(Path) ->
    Path1 = filename:absname(Path),
    Args = case os:type() of
               {unix, freebsd} ->
                   ["-c", "inotifywait $0 $@ & PID=$!; read a; kill $PID", "--",
                    "-m", "-e", "close_write", "-e", "moved_to", "-e", "create", "-e", "delete", "-r", Path1];
               {unix, linux} ->
                   ["-c", "inotifywait $0 $@ & PID=$!; read a; kill $PID",
                    "-m", "-e", "close_write", "-e", "moved_to", "-e", "create", "-e", "delete", "-r", Path1]
           end,
    erlang:open_port({spawn_executable, os:find_executable("sh")},
                     [stream, stderr_to_stdout, exit_status, {line, 16384}, {args, Args}]).

line_to_event(Line, RE) ->
    case re:run(Line, RE, [{capture, all_but_first, list}]) of
        {match, [Dir, Flags1, DirEntry]} ->
            Flags = [convert_flag(F) || F <- string:tokens(Flags1, ",")],
            Path = Dir ++ DirEntry,
            {Path, Flags};
        _ ->
            nomatch
    end.

line_parser() ->
    {ok, R} = re:compile("^(.*/) ([A-Z_,]+) (.*)$", [unicode]),
    R.

convert_flag("CREATE")      -> created;
convert_flag("DELETE")      -> deleted;
convert_flag("CLOSE_WRITE") -> modified;
convert_flag("CLOSE")       -> closed;
convert_flag("MOVED_TO")    -> moved;
convert_flag("ISDIR") -> isdir;
convert_flag(_)             -> undefined.
