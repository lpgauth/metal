-module(test_server2).
-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/1
]).

-behaviour(metal).
-export([
    handle_msg/2
]).

%% public
-spec start_link(atom()) ->
    {ok, pid()}.

start_link(Name) ->
    metal:start_link(?MODULE, Name, undefined).

%% metal callbacks
-spec handle_msg(term(), term()) ->
    {ok, term()}.

handle_msg({ping, Pid} , State) ->
    Pid ! pong,
    {ok, State}.
