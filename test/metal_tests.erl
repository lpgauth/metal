-module(metal_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CHILD(N), {N, ?START_LINK(N), permanent, 5000, worker, [?SERVER]}).
-define(SERVER, test_server).
-define(START_LINK(N), {?SERVER, start_link, [N]}).

metal_test() ->
    {ok, Pid} = test_server:start_link(server),
    {error, {already_started, Pid}} =
        test_server:start_link(server),

    ping(),
    exit(Pid, normal),
    {error, normal} = test_server:start_link(stop),
    {ok, Sup} = supervisor:start_link(test_sup, []),
    {ok, Pid2} = supervisor:start_child(Sup, ?CHILD(server)),
    {error, {already_started, Pid2}} =
        supervisor:start_child(Sup, ?CHILD(server)),

    ping(),
    server ! crash,
    timer:sleep(50),
    ping(),

    {test_server, _, Sup, _} = sys:get_state(server),
    ok = supervisor:terminate_child(Sup, server),
    ok = supervisor:delete_child(Sup, server),

    {ok, Pid3} = test_server2:start_link(server),
    ping(),
    exit(Pid3, normal),

    {ok, Pid4} = supervisor:start_child(Sup, ?CHILD(server)),
    server ! stop,
    timer:sleep(50),
    undefined = process_info(Pid4).

%% private
ping() ->
    server ! {ping, self()},
    receive pong ->
        ok
    end.
