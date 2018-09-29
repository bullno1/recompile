-module(recompile_scanner).
-behaviour(gen_server).
% API
-export([start_link/2]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-include_lib("kernel/include/file.hrl").

-record(state, { scan_interval :: pos_integer()
               , parent :: pid()
               , watch_paths :: list(string())
               , last_mtime :: calendar:datetime()
               }).

% API

start_link(ScanInterval, WatchPaths) ->
    gen_server:start_link(?MODULE, {self(), ScanInterval, WatchPaths}, []).

% gen_server

init({Parent, ScanInterval, WatchPaths}) ->
    State = #state{ parent = Parent
                  , scan_interval = ScanInterval
                  , watch_paths = WatchPaths
                  , last_mtime = calendar:local_time()
                  },
    {ok, State, ScanInterval}.

handle_call(_, _, State) -> {stop, unexpected, State}.

handle_cast(_, State) -> {stop, unexpected, State}.

handle_info(timeout, #state{ scan_interval = ScanInterval
                           , watch_paths = WatchPaths
                           , parent = Parent
                           , last_mtime = LastMtime
                           } = State) ->
    case first_change(WatchPaths, LastMtime) of
        none ->
            {noreply, State, ScanInterval};
        {File, Mtime} ->
            Parent ! {File, [modified]},
            {noreply, State#state{last_mtime = Mtime}, ScanInterval}
    end.

% Private

first_change([Path | Rest], LastMtime) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            FirstChange = first_change1(Path, Files, LastMtime),
            case FirstChange =/= none of
                true -> FirstChange;
                false -> first_change(Rest, LastMtime)
            end;
        {error, _} ->
            first_change(Rest, LastMtime)
    end;
first_change([], _LastMtime) ->
    none.

first_change1(Dir, [File | Rest], LastMtime) ->
    FilePath = filename:join(Dir, File),
    case file:read_file_info(FilePath) of
        {ok, #file_info{mtime = MTime}} when MTime > LastMtime ->
            {FilePath, MTime};
        _ ->
            first_change1(Dir, Rest, LastMtime)
    end;
first_change1(_, [], _LastMtime) ->
    none.
