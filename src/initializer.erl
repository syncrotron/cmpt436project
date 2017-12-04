-module(initializer).
-behaviour(supervisor).
-export([main/0]).
-export([init/1]).

main() ->
    domaintable:init(),
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
                    modules => [messagestore]},

                  #{id => messagehandler,
                    start => {messagehandler, start, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [messagehandler]},

                  #{id => domaintable,
                    start => {domaintable, start, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [domaintable]}

                  #{id => encoder,
                    start => {encoder, start, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [encoder]}],

    {ok, {SupFlags, ChildSpecs}}.
