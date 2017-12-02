{application, srouter,
    [{description, "Internet traffic router designed for high latency enviroments"},
    {vsn, "0.1.0"},
        {modules, [ %%%Update with all modules
            encoder, messagehandler, messagestore, parser
        ]},
    {applications, [stdlib, kernel, mnesia]}
]}.
