-module(metal).

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    init/4,
    start_link/3
]).

%% sys behavior
-export([
    system_code_change/4,
    system_continue/3,
    system_get_state/1,
    system_terminate/4
]).

%% callbacks
-callback init(Name :: atom(), Parent :: pid(), Opts :: term()) ->
    {ok, State :: term()} | {stop, Reason :: atom()}.

-callback handle_msg(Msg :: term(), State :: term()) ->
    {ok, State :: term()} | {stop, Reason :: atom()}.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

-optional_callbacks([
    init/3,
    terminate/2
]).

%% public
-spec start_link(module(), atom(), term()) ->
    {ok, pid()}.

start_link(Module, Name, State) ->
    proc_lib:start_link(?MODULE, init, [Module, Name, self(), State]).

-spec init(module(), atom(), pid(), term()) ->
    no_return() | ok.

init(Module, Name, Parent, State) ->
    case safe_register(Name) of
        true ->
            process_flag(trap_exit, true),
            module_init(Module, Name, Parent, State);
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% sys callbacks
-spec system_code_change(term(), module(), undefined | term(), term()) ->
    {ok, term()}.

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-spec system_continue(pid(), [], {module(), atom(), pid(), term()}) ->
    ok.

system_continue(_Parent, _Debug, {Module, Name, Parent, State}) ->
    loop(Module, Name, Parent, State).

-spec system_get_state(term()) ->
    {ok, term()}.

system_get_state(State) ->
    {ok, State}.

-spec system_terminate(term(), pid(), [], term()) ->
    none().

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

%% private
loop(Module, Name, Parent, State) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                {Module, Name, Parent, State});
        {'EXIT', Parent, Reason} ->
            terminate(Module, Reason, State);
        Msg ->
            case Module:handle_msg(Msg, State) of
                {ok, State2} ->
                    loop(Module, Name, Parent, State2);
                {stop, Reason} ->
                    terminate(Module, Reason, State)
            end
    end.

module_init(Module, Name, Parent, State) ->
    case erlang:function_exported(Module, init, 3) of
        true ->
            case Module:init(Name, Parent, State) of
                {ok, State2} ->
                    proc_lib:init_ack(Parent, {ok, self()}),
                    loop(Module, Name, Parent, State2);
                {stop, Reason} ->
                    proc_lib:init_ack(Parent, {error, Reason}),
                    exit(Reason)
            end;
        false ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(Module, Name, Parent, State)
    end.

safe_register(Name) ->
    try register(Name, self()) of
        true -> true
    catch
        _:_ -> {false, whereis(Name)}
    end.

terminate(Module, Reason, State) ->
    ok = case erlang:function_exported(Module, terminate, 2) of
        true ->
            Module:terminate(Reason, State);
        false ->
            ok
    end,
    exit(Reason).
