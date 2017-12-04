-module(initializer).
-behaviour(supervisor).
-export([main/0]).
-export([init/1]).

main() -> 
    supervisor:start_link(initializer, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [#{id => messagestore,
                    start => {messagestore, start, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [messagestore]}],
    {ok, {SupFlags, ChildSpecs}}.