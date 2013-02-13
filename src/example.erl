-module(example).
-export([proxy/1]).
-export([start/0]).

start() ->
    ranch_proxy:serve(100, ranch_tcp, [{port, 5555}], [{proxy, {?MODULE, proxy}}]).

proxy(_Data) ->
    {remote, {"localhost", 7777}}.
