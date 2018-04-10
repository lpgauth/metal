-module(test_server).
-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/1
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

%% public
-spec start_link(atom()) ->
    {ok, pid()}.

start_link(Name) ->
    metal:start_link(?MODULE, Name, undefined).

%% metal callbacks
-spec init(atom(), pid(), term()) ->
    {ok, term()}.

init(server, _Parent, _Opts) ->
    {ok, undefined};
init(stop, _Parent, _Opts) ->
    {stop, normal}.

-spec handle_msg(term(), term()) ->
    {ok, term()}.

handle_msg(crash, _State) ->
    exit(crash);
handle_msg(stop, _State) ->
    {stop, normal};
handle_msg({ping, Pid}, State) ->
    Pid ! pong,
    {ok, State}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, _State) ->
    ok.
