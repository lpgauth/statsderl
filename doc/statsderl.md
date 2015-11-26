

# Module statsderl #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#counter-3">counter/3</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-3">decrement/3</a></td><td></td></tr><tr><td valign="top"><a href="#gauge-3">gauge/3</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_decrement-3">gauge_decrement/3</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_increment-3">gauge_increment/3</a></td><td></td></tr><tr><td valign="top"><a href="#increment-3">increment/3</a></td><td></td></tr><tr><td valign="top"><a href="#timing-3">timing/3</a></td><td></td></tr><tr><td valign="top"><a href="#timing_fun-3">timing_fun/3</a></td><td></td></tr><tr><td valign="top"><a href="#timing_now-3">timing_now/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="counter-3"></a>

### counter/3 ###

<pre><code>
counter(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="decrement-3"></a>

### decrement/3 ###

<pre><code>
decrement(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="gauge-3"></a>

### gauge/3 ###

<pre><code>
gauge(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="gauge_decrement-3"></a>

### gauge_decrement/3 ###

<pre><code>
gauge_decrement(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="gauge_increment-3"></a>

### gauge_increment/3 ###

<pre><code>
gauge_increment(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="increment-3"></a>

### increment/3 ###

<pre><code>
increment(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="timing-3"></a>

### timing/3 ###

<pre><code>
timing(Key::iodata(), Value::number(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="timing_fun-3"></a>

### timing_fun/3 ###

<pre><code>
timing_fun(Key::iodata(), Fun::function(), SampleRate::float()) -&gt; ok
</code></pre>
<br />

<a name="timing_now-3"></a>

### timing_now/3 ###

<pre><code>
timing_now(Key::iodata(), Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>, SampleRate::float()) -&gt; ok
</code></pre>
<br />

