

# Module metal #
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `metal` behaviour.__<br /> Required callback functions: `handle_msg/2`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-4">init/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#system_code_change-4">system_code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#system_continue-3">system_continue/3</a></td><td></td></tr><tr><td valign="top"><a href="#system_get_state-1">system_get_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#system_terminate-4">system_terminate/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-4"></a>

### init/4 ###

<pre><code>
init(Module::module(), Name::atom(), Parent::pid(), State::term()) -&gt; no_return() | ok
</code></pre>
<br />

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Module::module(), Name::atom(), State::term()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="system_code_change-4"></a>

### system_code_change/4 ###

<pre><code>
system_code_change(State::term(), Module::module(), OldVsn::undefined | term(), Extra::term()) -&gt; {ok, term()}
</code></pre>
<br />

<a name="system_continue-3"></a>

### system_continue/3 ###

<pre><code>
system_continue(Parent::pid(), Debug::[], X3::{module(), atom(), pid(), term()}) -&gt; ok
</code></pre>
<br />

<a name="system_get_state-1"></a>

### system_get_state/1 ###

<pre><code>
system_get_state(State::term()) -&gt; {ok, term()}
</code></pre>
<br />

<a name="system_terminate-4"></a>

### system_terminate/4 ###

<pre><code>
system_terminate(Reason::term(), Parent::pid(), Debug::[], State::term()) -&gt; none()
</code></pre>
<br />

