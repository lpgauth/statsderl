# Changelog

## 0.7.2

### Added

- Telemetry events at the sample-rate gate. statsderl now emits two
  events that observers can attach to without instrumenting shackle
  internals:

  | Event | Measurements | Metadata | Fires when |
  |---|---|---|---|
  | `[statsderl, sample, sent]`    | `count => 1` | `operation => Operation` | A metric crosses the rate gate (or rate was 1/1.0) and is dispatched to shackle for UDP send. |
  | `[statsderl, sample, dropped]` | `count => 1` | `operation => Operation, rate => RateInt` | The rate gate suppresses a metric (`Rand > RateInt`). |

  Matches the telemetry pattern shackle (0.7.0) and whitecap already
  use. Event names are stable; the metadata map may grow over time.

- `telemetry` (1.4.2) is now a direct dependency. It was already
  pulled in transitively via shackle.

### Changed

- `statsderl_sample:rate_scaled/2` and the private `cast/1` helper
  now emit the telemetry events above. No behavioural change to
  return values or wire-format output; the events sit on the existing
  call paths.

## 0.7.1

### Changed

- Bumped `shackle` dependency from `0.7.0` to `0.7.1`, which itself
  bumps `metal 0.1.1 -> 0.1.2` and `foil 0.1.3 -> 0.1.4` (both
  infrastructure-only refreshes, plus a tightened `error/0` type
  and a macro-based DRY refactor in foil).

`0.7.0` was tagged in git but its hex publish failed (the granderl
removal commit landed past hex's one-hour modification window for
the pre-existing 0.7.0 release). `0.7.1` is the first hex release
to ship the granderl-to-knot swap; behaviour and API are
identical to the `0.7.0` git tag.

## 0.7.0

### Breaking

- Minimum supported Erlang/OTP version is now **25**, inherited from
  `shackle 0.7.0`.

### Changed

- Replaced the `granderl` dependency with **`knot`** (in-house C NIF
  using wyrand). `statsderl_sample:rate_scaled/2` now calls
  `knot:uniform/1` — same signature, same range guarantees, no
  behavioural change at the sample-rate gate.
- Bumped `shackle` from git ref `0.6.16` to the hex `0.7.0` release.
- Bumped `parse_trans` from `3.0.0` to `3.4.2`.
- CI matrix now covers OTP 25, 26, 27, 28; `actions/checkout` upgraded
  to v5.
- Documentation migrated from `edown` to `rebar3_ex_doc` (matching
  shackle 0.7.0). Generated `doc/` directory removed from the repo;
  HTML docs are now published to hexdocs on release.

### Why

`granderl 0.1.5` on hex.pm fails to build on OTP 27+: its `preamble.sh`
invokes `erl -noshell -s init stop -eval ...` with options in an order
that causes the node to terminate before `-eval` runs, leaving
`ERTS_INCLUDE_DIR` empty and the C compiler unable to find `erl_nif.h`.
The fix exists in granderl's master but the hex package is not under
our ownership. `knot` is a small in-house C NIF (~80 LOC) that
implements the same `uniform/1` primitive cleanly on every supported
OTP.
