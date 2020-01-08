

# Module statsderl_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-base_key">base_key()</a> ###


<pre><code>
base_key() = <a href="#type-base_key_part">base_key_part()</a> | [<a href="#type-base_key_part">base_key_part()</a>]
</code></pre>




### <a name="type-base_key_part">base_key_part()</a> ###


<pre><code>
base_key_part() = hostname | name | sname | undefined | iodata()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#base_key-1">base_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#error_msg-2">error_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td></td></tr><tr><td valign="top"><a href="#timing_now-1">timing_now/1</a></td><td></td></tr><tr><td valign="top"><a href="#timing_now_us-1">timing_now_us/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="base_key-1"></a>

### base_key/1 ###

<pre><code>
base_key(Key::<a href="#type-base_key">base_key()</a>) -&gt; iodata()
</code></pre>
<br />

<a name="error_msg-2"></a>

### error_msg/2 ###

<pre><code>
error_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

<a name="timestamp-0"></a>

### timestamp/0 ###

<pre><code>
timestamp() -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

<a name="timing_now-1"></a>

### timing_now/1 ###

<pre><code>
timing_now(Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="timing_now_us-1"></a>

### timing_now_us/1 ###

<pre><code>
timing_now_us(Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

