# Changelog

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
