# statsderl

High Performance StatsD Erlang client

[![Build Status](https://travis-ci.org/lpgauth/statsderl.svg?branch=master)](https://travis-ci.org/lpgauth/statsderl)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/statsderl/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/statsderl?branch=master)

### Requirements

* Erlang 16.0 +

### Environment variables

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>base_key</td>
    <td>base_key()</td>
    <td>undefined</td>
    <td>key prefix</td>
  </tr>
  <tr>
    <td>hostname</td>
    <td>inet:ip_address() | inet:hostname() | binary()</td>
    <td>{127,0,0,1}</td>
    <td>server hostname</td>
  </tr>
  <tr>
    <td>port</td>
    <td>inet:port_number()</td>
    <td>8125</td>
    <td>server port</td>
  </tr>
</table>

#### base_key options

<table width="100%">
  <theader>
    <th>Option</th>
    <th>Source</th>
    <th>Description</th>
    <th>Example</th>
  </theader>
  <tr>
    <td>hostname</td>
    <td>inet:gethostname/0</td>
    <td>use the hostname</td>
    <td>"h033"</td>
  </tr>
  <tr>
    <td>name</td>
    <td>erl -name</td>
    <td>use the long node name</td>
    <td>"nonode@nohost"</td>
  </tr>
  <tr>
    <td>sname</td>
    <td>erl -sname</td>
    <td>use the short node name</td>
    <td>"nonode"</td>
  </tr>
</table>

## API
<a href="https://github.com/lpgauth/statsderl/blob/master/doc/statsderl.md#index" class="module">Function Index</a>

## Examples

```erlang
1> statsderl_app:start().
ok.

2> statsderl:counter(["test", $., "counter"], 1, 0.23).
ok.

3> statsderl:decrement("test.decrement", 1, 0.5).
ok.

4> statsderl:gauge([<<"test">>, $., "gauge"], 333, 1.0).
ok.

5> statsderl:gauge_decrement([<<"test.gauge_decrement">>], 15, 0.001).
ok.

6> statsderl:gauge_increment(<<"test.gauge_increment">>, 32, 1).
ok.

7> statsderl:increment(<<"test.increment">>, 1, 1).
ok.

8> statsderl:timing("test.timing", 5, 0.5).
ok.

9> statsderl:timing_fun(<<"test.timing_fun">>, fun() -> timer:sleep(100) end, 0.5).
ok.

10> Timestamp = os:timestamp().
{1448,591778,258983}

11> statsderl:timing_now("test.timing_now", Timestamp, 0.15).
ok.

12> statsderl_app:stop().
ok.
```

## Tests

```makefile
make test
```

### TODOs

* NIF for faster random
* parse transform for severity levels (debug, development, production)
* parse transform for type conversion

## License

```license
Copyright (c) 2011-2015, Louis-Philippe Gauthier
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the BLOOM Digital Platforms nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL BLOOM DIGITAL PLATFORMS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
