%%%Created by Sam Horovatin
-module(test).
-export([all/0]).

all() ->
    ct:run_test([{spec, "tests.spec"}]),
    {Osf, _} = os:type(),
    case Osf of
         win32 -> os:cmd("start logs/index.html");
         unix -> os:cmd("xdg-open logs/index.html");
         ose -> os:cmd("xdg-open logs/index.html")
    end.
