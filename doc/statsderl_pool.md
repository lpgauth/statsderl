

# Module statsderl_pool #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = iodata()
</code></pre>




### <a name="type-operation">operation()</a> ###


<pre><code>
operation() = {cast, iodata()} | {counter, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>, <a href="#type-sample_rate">sample_rate()</a>} | {gauge, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>} | {gauge_decrement, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>} | {gauge_increment, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>} | {timing, <a href="#type-key">key()</a>, <a href="#type-value">value()</a>} | {timing_now, <a href="#type-key">key()</a>, <a href="erlang.md#type-timestamp">erlang:timestamp()</a>} | {timing_now_us, <a href="#type-key">key()</a>, <a href="erlang.md#type-timestamp">erlang:timestamp()</a>}
</code></pre>




### <a name="type-pool_size">pool_size()</a> ###


<pre><code>
pool_size() = pos_integer()
</code></pre>




### <a name="type-sample_rate">sample_rate()</a> ###


<pre><code>
sample_rate() = number()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = number()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#sample-2">sample/2</a></td><td></td></tr><tr><td valign="top"><a href="#sample_scaled-2">sample_scaled/2</a></td><td></td></tr><tr><td valign="top"><a href="#server_name-1">server_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#size-0">size/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

<a name="sample-2"></a>

### sample/2 ###

<pre><code>
sample(Rate::<a href="#type-sample_rate">sample_rate()</a>, Operation::<a href="#type-operation">operation()</a>) -&gt; ok
</code></pre>
<br />

<a name="sample_scaled-2"></a>

### sample_scaled/2 ###

<pre><code>
sample_scaled(RateInt::non_neg_integer(), Operation::<a href="#type-operation">operation()</a>) -&gt; ok
</code></pre>
<br />

<a name="server_name-1"></a>

### server_name/1 ###

<pre><code>
server_name(N::pos_integer()) -&gt; atom()
</code></pre>
<br />

<a name="size-0"></a>

### size/0 ###

<pre><code>
size() -&gt; <a href="#type-pool_size">pool_size()</a>
</code></pre>
<br />

