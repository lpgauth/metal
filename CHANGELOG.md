# Changelog

## 0.1.2

Pure infrastructure modernization. No source or API changes.

### Changed

- CI moved from Travis (decommissioned years ago) to GitHub Actions.
  Matrix now covers OTP 25, 26, 27, 28.
- Documentation migrated from `edown` to `rebar3_ex_doc` (matching
  shackle, statsderl, etc.). Generated `doc/` directory removed from
  the repo; HTML docs are now published to hexdocs on release.

### Removed

- `.travis.yml`, `elvis.config`, `bin/elvis`, `rebar.config.script`
  (rebar2 compatibility) — all unused.
- `coveralls` plugin and Makefile target — Travis-specific tooling.
- Coveralls and Travis build badges from the README.
