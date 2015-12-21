

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#base_key-1">base_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#error_msg-2">error_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#getaddrs-1">getaddrs/1</a></td><td></td></tr><tr><td valign="top"><a href="#random-1">random/1</a></td><td></td></tr><tr><td valign="top"><a href="#random_element-1">random_element/1</a></td><td></td></tr><tr><td valign="top"><a href="#random_server-0">random_server/0</a></td><td></td></tr><tr><td valign="top"><a href="#server_name-1">server_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-0">timestamp/0</a></td><td></td></tr></table>


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

<a name="getaddrs-1"></a>

### getaddrs/1 ###

<pre><code>
getaddrs(Address::<a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>) -&gt; {ok, <a href="inet.md#type-ip_address">inet:ip_address()</a>} | {error, atom()}
</code></pre>
<br />

<a name="random-1"></a>

### random/1 ###

<pre><code>
random(N::pos_integer()) -&gt; pos_integer()
</code></pre>
<br />

<a name="random_element-1"></a>

### random_element/1 ###

<pre><code>
random_element(List::[term()]) -&gt; term()
</code></pre>
<br />

<a name="random_server-0"></a>

### random_server/0 ###

<pre><code>
random_server() -&gt; atom()
</code></pre>
<br />

<a name="server_name-1"></a>

### server_name/1 ###

<pre><code>
server_name(X1::pos_integer()) -&gt; atom()
</code></pre>
<br />

<a name="timestamp-0"></a>

### timestamp/0 ###

<pre><code>
timestamp() -&gt; <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>
<br />

