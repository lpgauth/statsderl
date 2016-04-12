

# Module statsderl #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Functions to send data to a StatsD server.

<a name="description"></a>

## Description ##
In this module, the
SampleRate parameter is a float value between 0 and 1 that
indicates how frequently the data should actually be sent. For
counters, the server takes the sample rate into account.
<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = iodata()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#counter-3">counter/3</a></td><td>Increment a counter (identical to increment/3).</td></tr><tr><td valign="top"><a href="#decrement-3">decrement/3</a></td><td>Decrement a counter.</td></tr><tr><td valign="top"><a href="#gauge-3">gauge/3</a></td><td>Set a gauge value.</td></tr><tr><td valign="top"><a href="#gauge_decrement-3">gauge_decrement/3</a></td><td>Decrement a gauge value.</td></tr><tr><td valign="top"><a href="#gauge_increment-3">gauge_increment/3</a></td><td>Increment a gauge value.</td></tr><tr><td valign="top"><a href="#increment-3">increment/3</a></td><td>Increment a counter.</td></tr><tr><td valign="top"><a href="#timing-3">timing/3</a></td><td>Record timer information.</td></tr><tr><td valign="top"><a href="#timing_fun-3">timing_fun/3</a></td><td>Run nullary function and record the time spent.</td></tr><tr><td valign="top"><a href="#timing_now-3">timing_now/3</a></td><td>Record time spent between the timestamp passed and now.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="counter-3"></a>

### counter/3 ###

<pre><code>
counter(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Increment a counter (identical to increment/3).

<a name="decrement-3"></a>

### decrement/3 ###

<pre><code>
decrement(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Decrement a counter.

<a name="gauge-3"></a>

### gauge/3 ###

<pre><code>
gauge(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Set a gauge value.

<a name="gauge_decrement-3"></a>

### gauge_decrement/3 ###

<pre><code>
gauge_decrement(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Decrement a gauge value.

<a name="gauge_increment-3"></a>

### gauge_increment/3 ###

<pre><code>
gauge_increment(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Increment a gauge value.

<a name="increment-3"></a>

### increment/3 ###

<pre><code>
increment(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Increment a counter.

<a name="timing-3"></a>

### timing/3 ###

<pre><code>
timing(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Record timer information.

<a name="timing_fun-3"></a>

### timing_fun/3 ###

<pre><code>
timing_fun(Key::<a href="#type-key">key()</a>, Fun::function(), SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Run nullary function and record the time spent.

<a name="timing_now-3"></a>

### timing_now/3 ###

<pre><code>
timing_now(Key::<a href="#type-key">key()</a>, Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; ok
</code></pre>
<br />

Record time spent between the timestamp passed and now.

