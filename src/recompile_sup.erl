-module(recompile_sup).
-behaviour(supervisor).
% API
-export([start_link/0]).
% supervisor
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Env = application:get_all_env(recompile),
    Flags = #{ strategy => one_for_one
             , intensity => 1
             , period => 5
             },
    Children = [#{ id => recompile
                 , start => {recompile, start_link, [Env]}
                 }],
    {ok, {Flags, Children}}.
