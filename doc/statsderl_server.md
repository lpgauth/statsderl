

# Module statsderl_server #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`metal`](metal.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle_msg-2">handle_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle_msg-2"></a>

### handle_msg/2 ###

<pre><code>
handle_msg(X1::term(), State::term()) -&gt; {ok, term()}
</code></pre>
<br />

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(Name::atom(), Parent::pid(), Opts::term()) -&gt; {ok, term()} | {stop, atom()}
</code></pre>
<br />

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Name::atom()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::term(), State::term()) -&gt; ok
</code></pre>
<br />

