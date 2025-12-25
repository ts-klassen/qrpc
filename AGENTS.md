# Repository Guidelines

## Project Structure & Module Organization
- `apps/qrpc/` is the main OTP application (Erlang source in `src/`, includes in `include/`, tests in `test/`, static assets in `priv/static/`). Also called framework. The word subsystem may also include framework.
- `apps/q_tut/` and `apps/q_vvx/` are additional OTP apps with their own `src/`, `priv/`, and `ChangeLog`. Also called subsystem.
- `config/` holds runtime configs (`sys.config`, `vm.args`), system service units (`config/system/`), and pkgsrc overlays (`config/pkgsrc/`).
- `scripts/` and the top-level `Build` script provide build, packaging, and utility automation.
- `examples/`, `ansible/`, and `aws/` contain sample clients and infrastructure tooling.

## Binary PATH

This project uses custom packages installed under `/opt/qrpc/pkg/bin`.
- /opt/qrpc/pkg/bin/erl
- /opt/qrpc/pkg/bin/erlc
- /opt/qrpc/pkg/bin/rebar3
- /opt/qrpc/pkg/bin/cargo

## Build, Test, and Development Commands
- `/opt/qrpc/pkg/bin/rebar3 compile` or `./Build build` — compile Erlang apps.
- `./Build check` — run EUnit, Common Test, and coverage (`rebar3 eunit`, `rebar3 ct --verbose`, `rebar3 cover`).
- NEVER use other Build args.

## Testing Guidelines
- Common Test suites live in `apps/*/test/*_SUITE.erl`; EUnit tests use `*_tests.erl`.
- Only add tests if asked.

## ChangeLog
- If you added a new feature, fixed a bug in an existing release, or have breaking changes, you MUST update the ChangeLog. Use your changelog skills.
- If you fixed a bug before release, do NOT add that fix to ChangeLog.

## Commit Guidelines
- Only make a git commit if asked. If you are asked to make a git commit, you are suppose to make a single git commit.
- You may only commit files that are changed when commit requested. If you edit files after the commit request, you must wait for another commit request to commit them.
- Commit message should be a one line English within 80 characters.

## Release Guidelines
- Unless you are told to make a release:
  - Do NOT add a git tag.
  - Do NOT use the term `bump version to` in your git commit message.
