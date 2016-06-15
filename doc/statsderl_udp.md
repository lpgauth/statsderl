

# Module statsderl_udp #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#header-2">header/2</a></td><td></td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="header-2"></a>

### header/2 ###

<pre><code>
header(IP::<a href="inet.md#type-ip_address">inet:ip_address()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; iodata()
</code></pre>
<br />

<a name="send-3"></a>

### send/3 ###

<pre><code>
send(Socket::<a href="inet.md#type-socket">inet:socket()</a>, Header::iodata(), Data::iodata()) -&gt; ok | {error, term()}
</code></pre>
<br />

