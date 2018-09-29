-module(recompile).
-behaviour(application).
-behaviour(gen_statem).
% API
-export([start_link/1]).
% application
-export([start/2, stop/1]).
% gen_server
-export([init/1, callback_mode/0, idle/3, debouncing/3]).

-record(data, { watch_paths :: [string()]
              , compile_dir :: string()
              , debounce_time :: pos_integer()
              , compile_cmd :: string()
              }).

% API

start_link(Opts) ->
    gen_statem:start_link(
      {local, ?MODULE}, ?MODULE, Opts, [{hibernate_after, 1000}]
     ).

% application

start(_StartType, _StartArgs) -> recompile_sup:start_link().

stop(_State) -> ok.

% gen_statem

callback_mode() -> state_functions.

init(Opts) ->
    case watch_modules() of
        {ok, WatchPaths} ->
            Data = #data{
                watch_paths = WatchPaths,
                debounce_time = proplists:get_value(debounce_time, Opts),
                compile_dir = find_compile_dir(WatchPaths),
                compile_cmd = proplists:get_value(compile_cmd, Opts)
             },
            {ok, idle, Data};
        {error, Reason} ->
            {stop, Reason}
    end.

idle(info, {Path, _Events}, #data{debounce_time = DebounceTime} = Data)  ->
    DebounceTimeout = {state_timeout, DebounceTime, recompile},
    error_logger:info_report([ {recompile, modified}
                             , {path, Path}
                             ]),
    {next_state, debouncing, Data, [DebounceTimeout]};
idle(_, _, _Data) ->
    keep_state_and_data.

debouncing(info, {Path, _Events}, _Data) ->
    error_logger:info_report([ {recompile, modified}
                             , {path, Path}
                             ]),
    keep_state_and_data;
debouncing(state_timeout, recompile, Data) ->
    recompile(Data#data.compile_cmd, Data#data.compile_dir),
    Modules = [Module || {module, Module} <- c:lm()],
    error_logger:info_report([ {recompile, reloaded}
                             , {modules, Modules}
                             ]),
    lists:foreach(fun call_reload_triggers/1, Modules),
    {next_state, idle, Data};
debouncing(_, _, _Data) ->
    keep_state_and_data.

% Private

watch_modules() ->
    Apps = [App || {App, _, _} <- application:loaded_applications()],
    Modules = lists:flatmap(
                fun(App) ->
                    case application:get_key(App, modules) of
                        {ok, Modules} -> Modules;
                        _ -> []
                    end
                end,
                Apps
               ),
    ModulePaths = lists:filtermap(
                    fun(Module) ->
                        CompileInfo = Module:module_info(compile),
                        case proplists:get_value(source, CompileInfo) of
                            undefined -> false;
                            Source -> {true, filename:dirname(Source)}
                        end
                    end,
                    Modules
                   ),
    WatchPaths = lists:filter(fun filelib:is_dir/1, lists:usort(ModulePaths)),
    case watch_paths(WatchPaths) of
        ok -> {ok, WatchPaths};
        {error, _} = Err -> Err
    end.

watch_paths([Path | Rest]) ->
    case enotify:start_link(Path, [modified]) of
        {ok, _} -> watch_paths(Rest);
        {error, _} = Err -> Err
    end;
watch_paths([]) ->
    ok.

find_compile_dir(WatchPaths) ->
    RebarAppPaths = lists:usort(
        lists:filtermap(
            fun(Path) ->
                RealPath = resolve_link(Path),
                AppDir = filename:dirname(RealPath),
                RebarConf = filename:join(AppDir, "rebar.config"),
                case filelib:is_regular(RebarConf) of
                    true -> {true, AppDir};
                    false -> false
                end
            end,
            WatchPaths
         )
     ),
    [CompileDir | _] = RebarAppPaths,
    CompileDir.

% Lifted from rebar3
canonical_path(Dir) ->
    Canon = canonical_path([], filename:split(filename:absname(Dir))),
    filename:nativename(Canon).

canonical_path([], [])                -> filename:absname("/");
canonical_path(Acc, [])               -> filename:join(lists:reverse(Acc));
canonical_path(Acc, ["."|Rest])       -> canonical_path(Acc, Rest);
canonical_path([_|Acc], [".."|Rest])  -> canonical_path(Acc, Rest);
canonical_path([], [".."|Rest])       -> canonical_path([], Rest);
canonical_path(Acc, [Component|Rest]) -> canonical_path([Component|Acc], Rest).

resolve_link(Path) ->
    case file:read_link(Path) of
        {ok, Target} ->
            canonical_path(filename:absname(Target, filename:dirname(Path)));
        {error, _} -> Path
    end.

recompile([Exe | Args], Dir) ->
    ExePath = os:find_executable(Exe),
    Port = erlang:open_port(
      {spawn_executable, ExePath},
      [ {args, Args}
      , {line, 2041}
      , {cd, Dir}
      , stderr_to_stdout
      , binary
      , in
      , eof
      ]
     ),
    receive_loop(Port).

receive_loop(Port) ->
    receive
        {Port, {data, {Eol, Line}}} ->
            EndChar =
                case Eol of
                    eol -> <<"\n">>;
                    noeol -> <<>>
                end,
            io:format("~s~s", [Line, EndChar]),
            receive_loop(Port);
        {Port, eof} -> ok
    end.

call_reload_triggers(Module) ->
    Attributes = Module:module_info(attributes),
    Triggers = proplists:get_value(on_reload, Attributes, []),
    lists:foreach(
      fun({Function, 0}) ->
          Result = (catch apply(Module, Function, [])),
          error_logger:info_report([ {recompile, reload_trigger}
                                   , {module, Module}
                                   , {function, Function}
                                   , {result, Result}
                                   ]);
         (_) -> ok
      end,
      Triggers
     ).
