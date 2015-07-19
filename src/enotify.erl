%% @doc
%% enotify takes a path and list of filesystem events for that path
%% to send notifications about to the calling process.
%%
%% ```
%% > enotify:start_link("./", [created, closed, modified, moved, deleted, is_dir, undefined]).
%% > flush().
%% Shell got {".../enotify/src/README.md",[closed,modified]}}'''
%%

-module(enotify).

-behaviour(gen_server).

-export([start_link/0
        ,start_link/1
        ,start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-define(SERVER, ?MODULE).

-define(EVENTS, [created, closed, modified, moved, deleted, is_dir, undefined]).

-type event() :: created | closed | modified | moved | deleted | is_dir | undefined.
-record(state, { port    :: port(),
                 path    :: file:filename_all(),
                 re      :: re:mp(),
                 events  :: [event()],
                 backend :: fsevents | inotifywait | inotifywait_win32,
                 pid     :: pid() }).

start_link() ->
    start_link("", ?EVENTS).

start_link(Path) ->
    start_link(Path, ?EVENTS).

start_link(Path, Events) ->
    case os:type() of
        {unix, darwin} ->
            gen_server:start_link(?MODULE, [enotify_fsevents, Path, Events, self()], []);
        {unix, linux} ->
            gen_server:start_link(?MODULE, [enotify_inotifywait, Path, Events, self()], []);
        {win32, nt} ->
            gen_server:start_link(?MODULE, [enotify_inotifywait_win32, Path, Events, self()], []);
        _ ->
            {error, no_backend}
    end.

init([Backend, Path, Events, Pid]) ->
    {ok, #state{port=Backend:start_port(Path)
               ,re=Backend:line_parser()
               ,path=Path
               ,events=sets:from_list(Events)
               ,backend=Backend
               ,pid=Pid}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Port, {data, {eol, Line}}}, #state{backend=Backend
                                                ,re=RE
                                                ,events=ListenEvents
                                                ,pid=Pid}=State) ->
    {EventPath, Events} = Backend:line_to_event(Line, RE),
    case sets:is_disjoint(ListenEvents, sets:from_list(Events)) of
        true ->
            {noreply, State};
        false ->
            Pid ! {EventPath, Events},
            {noreply, State}
    end;
handle_info({_Port, {data, {noeol, Line}}}, State) ->
    error_logger:error_msg("~p line too long: ~p, ignoring~n", [?SERVER, Line]),
    {noreply, State};
handle_info({_Port, {exit_status, Status}}, State) ->
    {stop, {port_exit, Status}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port=Port}) ->
    (catch port_close(Port)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
