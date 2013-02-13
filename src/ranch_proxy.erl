-module(ranch_proxy).
-behaviour(ranch_protocol).

%% API
-export([serve/4]).

%% Behaviour callbacks
-export([start_link/4, init/4]).

%% Defaults
-define(DEFAULT_SOCKET_OPTS, [{packet, 0}, {active, once}]).
-define(DEFAULT_TIMEOUT, 5000).

%% Internal state
-record(state, {
        socket                                    :: inet:socket(),
        socket_opts        = ?DEFAULT_SOCKET_OPTS :: list(gen_tcp:option()),
        transport                                 :: module(),
        proxy                                     :: {module(), function()},
        buffer             = <<>>                 :: binary(),
        remote_endpoint                           :: any(),
        remote_socket                             :: inet:socket(),
        remote_transport                          :: module(),
        remote_socket_opts = ?DEFAULT_SOCKET_OPTS :: list(gen_tcp:option()),
        remote_connect_fun = fun remote_connect/1 :: function(),
        timeout                                   :: non_neg_integer()
    }).

%% ----------------------------------------------------------
%% API
%% ----------------------------------------------------------

serve(Listeners, Protocol, ProtocolOpts, ProxyOpts) ->
    {ok, _} = ranch:start_listener(?MODULE, Listeners, Protocol,
                                   ProtocolOpts, ?MODULE, ProxyOpts).

%% ----------------------------------------------------------
%% Callbacks
%% ----------------------------------------------------------

start_link(ListenerPid, Socket, Transport, Opts) ->
        Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
        {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
        ok       = ranch:accept_ack(ListenerPid),
        {M, F}   = proplists:get_value(proxy, Opts),
        Timeout  = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
        SOpts    = proplists:get_value(source_opts, Opts, ?DEFAULT_SOCKET_OPTS),
        ROpts    = proplists:get_value(remote_opts, Opts, ?DEFAULT_SOCKET_OPTS),
        RConnFun = proplists:get_value(remote_connect, Opts, fun remote_connect/1),

        loop(#state{
                socket             = Socket,
                transport          = Transport,
                proxy              = {M, F},
                timeout            = Timeout,
                socket_opts        = SOpts,
                remote_socket_opts = ROpts,
                remote_connect_fun = RConnFun
            }).

%% ----------------------------------------------------------
%% Proxy internals
%% ----------------------------------------------------------

loop(State = #state{ socket    = Socket,
                     transport = Transport,
                     proxy     = Proxy,
                     buffer    = Buffer,
                     timeout   = Timeout }) ->
        case Transport:recv(Socket, 0, Timeout) of
                {ok, Data} ->
                        Buffer1 = <<Buffer/binary, Data/binary>>,
                        case run_proxy(Proxy, Buffer1) of
                            stop ->
                                terminate(State);
                            ignore ->
                                loop(State);
                            {buffer, NewData} ->
                                loop(State#state{ buffer = NewData });
                            {remote, Remote} ->
                                start_proxy_loop(State#state{
                                    buffer          = Buffer1,
                                    remote_endpoint = Remote
                                });
                            [{remote, Remote}, {data, NewData}] ->
                                start_proxy_loop(State#state{
                                    buffer          = NewData,
                                    remote_endpoint = Remote
                                });
                            [{remote, Remote}, {data, NewData}, {reply, Reply}] ->
                                Transport:send(Socket, Reply),
                                start_proxy_loop(State#state{
                                    buffer          = NewData,
                                    remote_endpoint = Remote
                                });
                            _ ->
                                loop(State#state{ buffer = Buffer1 })
                        end;
                _ ->
                        terminate(State)
        end.

start_proxy_loop(State = #state{ remote_endpoint = Remote, buffer = Buffer }) ->
    case remote_connect(Remote) of
        {Transport, {ok, Socket}} ->
            Transport:send(Socket, Buffer),
            proxy_loop(State#state{ remote_socket = Socket,
                                    remote_transport = Transport,
                                    buffer = <<>> });
        {_, {error, _Error}} ->
            terminate(State)
    end.

proxy_loop(State = #state{ socket             = SSock,
                           transport          = STrans,
                           socket_opts        = SOpts,
                           remote_socket      = RSock,
                           remote_transport   = RTrans,
                           remote_socket_opts = ROpts }) ->

    STrans:setopts(SSock, SOpts),
    RTrans:setopts(RSock, ROpts),

    receive
        {_, SSock, Data} ->
            RTrans:send(RSock, Data),
            proxy_loop(State);
        {_, RSock, Data} ->
            STrans:send(SSock, Data),
            proxy_loop(State);
        {tcp_closed, RSock} ->
            terminate(State);
        {tcp_closed, SSock} ->
            terminate_remote(State);
        _ ->
            terminate_all(State)
    end.

remote_connect({Ip, Port}) ->
    {ranch_tcp, gen_tcp:connect(Ip, Port, [binary,
                {packet, 0}, {delay_send, true}])}.

run_proxy({M, F}, Data) ->
    M:F(Data).

terminate(#state{ socket = Socket, transport = Transport }) ->
    Transport:close(Socket).

terminate_remote(#state{remote_socket = Socket, remote_transport = Transport}) ->
    Transport:close(Socket),
    ok.

terminate_all(State) ->
    terminate_remote(State),
    terminate(State).


