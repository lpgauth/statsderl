# statsderl

High Performance Erlang StatsD Client

[![Build Status](https://travis-ci.org/lpgauth/statsderl.svg?branch=master)](https://travis-ci.org/lpgauth/statsderl)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/statsderl/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/statsderl?branch=master)

### Requirements

* Erlang 16.0 +

### Features

* Performance optimized

## API
<a href="https://github.com/lpgauth/statsderl/blob/master/doc/statsderl.md#index" class="module">Function Index</a>

#### Environment variables

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
    <td>inet:ip_address() | inet:hostname() | binary() | [{inet:ip_address() | inet:hostname() | binary(), inet:port_number()}]</td>
    <td>{127,0,0,1}</td>
    <td>server hostname</td>
  </tr>
  <tr>
    <td>port</td>
    <td>inet:port_number()</td>
    <td>8125</td>
    <td>server port. Not needed when using the list syntax in the hostname variable</td>
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

* parse transform for type conversion

## License

```license
The MIT License (MIT)

Copyright (c) 2011-2016 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
