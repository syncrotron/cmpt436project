{application, srouter,
    [{description, "Internet traffic router designed for high latency enviroments"},
    {vsn, "0.1.0"},
        {modules, [ %%%Update with all modules
        domaintable_SUITE.erl
        ]},
    {applications, [stdlib, kernel, mnesia]}
]}.
