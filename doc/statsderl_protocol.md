

# Module statsderl_protocol #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = iodata()
</code></pre>




### <a name="type-op_code">op_code()</a> ###


<pre><code>
op_code() = decrement | gauge | gauge_decrement | gauge_increment | increment | timing
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#encode-4">encode/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="encode-4"></a>

### encode/4 ###

<pre><code>
encode(X1::<a href="#type-op_code">op_code()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, SampleRate::<a href="#type-sample_rate">sample_rate()</a>) -&gt; iodata()
</code></pre>
<br />

