# metal

Lightweight Erlang server behaviour

[![Build Status](https://travis-ci.org/lpgauth/metal.svg?branch=master)](https://travis-ci.org/lpgauth/metal)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/metal/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/metal?branch=master)

## API
### Behaviour callbacks

#### Required

```erlang
-callback handle_msg(Msg :: term(), State :: term()) ->
    {ok, State :: term()}.
```

#### Optional

```erlang
-callback init(Name :: atom(), Parent :: pid(), State :: term()) ->
    {ok, State :: term()}.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.
```
## Example
```erlang
-module(test_server).

-export([
    start_link/1
]).

-behaviour(metal).
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

start_link(Name) ->
    metal:start_link(?MODULE, Name, Opts).

%% metal callbacks
init(_Name, _Parent, Opts) ->
    {ok, State}.

handle_msg(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
```

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```
## License
```license
The MIT License (MIT)

Copyright (c) 2016-2017 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
