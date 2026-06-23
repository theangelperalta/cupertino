# cupertino

A CLI tool for Apple 🍎 platform development workflows — a friendlier front end
to `xcodebuild` that runs schemes against destinations as a matrix, in parallel,
behind a live progress dashboard.

## Features

- **Scheme × destination matrix** — build, test, or clean one scheme against many
  destinations (or many schemes at once) from a single command.
- **Parallel cells** — run each scheme@destination *cell* concurrently, each with
  its own isolated DerivedData so they don't collide (`-j` to cap concurrency).
- **Live dashboard** — a continuously updated view of in-progress actions,
  warnings/errors, slow tasks, and an optional cache-hit percentage.
- **Swift Build (SWB) interception** — optionally intercept the Swift Build wire
  protocol for structured progress events instead of scraping `xcodebuild` text
  output (`--use-swb`).
- **Install with log streaming** — build and install an app to a simulator or
  device and stream its logs with Xcode-parity formatting.
- **Project-local configuration** — defaults (schemes, destinations, dashboard
  tuning) are stored per project in `.cupertino/cupertino.lisp`.

## Requirements

- macOS with **Xcode** and its command-line tools (`xcodebuild`, `xcrun`,
  `simctl`).
- **SBCL** and **[Roswell](https://github.com/roswell/roswell)** (`ros`) to build
  the binary.
- **[qlot](https://github.com/fukamachi/qlot)** for dependency management.

## Build & install

```sh
make build      # qlot install + build the binary at roswell/cupertino
make install    # symlink the binary into ~/.roswell/bin/cupertino
make verify     # confirm cupertino is on PATH
```

`make install` puts `cupertino` on your `PATH` (assuming `~/.roswell/bin` is in
it).

## Quick start

```sh
cupertino init                       # interactively set project defaults
cupertino build                      # build using configured scheme(s)/destination(s)
cupertino test                       # run tests
cupertino install --sim <UDID>       # build & install to a simulator, then stream logs
```

Once defaults are configured you can usually run `cupertino build` / `test` /
`clean` with no flags.

## Commands

| Command   | Description |
|-----------|-------------|
| `init`    | Interactively set up project configuration. |
| `config`  | View or update project configuration (schemes, destinations, dashboard tuning). |
| `build`   | Build the Xcode project across the scheme × destination matrix. |
| `test`    | Run tests; supports `--only-testing` / `--skip-testing` filters. |
| `clean`   | Clean build products; `--prune` also removes cupertino's per-cell DerivedData. |
| `install` | Build and install the app to a simulator or device and stream its logs. |
| `info`    | Display project, simulator, or physical-device information (`info project` / `info sim` / `info device`). |
| `dot`     | Generate a tree representation of the project model in Graphviz Dot format. |

Run `cupertino <command> --help` for the full option list.

### Cells (scheme × destination)

A *cell* is one `scheme@destination` pairing. You can drive the matrix in two ways:

- Repeat `-s/--scheme` together with `--sim` / `--device` to form the cartesian
  product of schemes and destinations.
- Pass explicit `--cell` values to bypass the matrix, e.g.:

```sh
cupertino build --cell 'MyApp@sim=ABC123' --cell 'MyApp@device=UDID9'
cupertino test  -s MyApp --sim ABC123 --sim DEF456 -j 2
```

Common flags shared by `build` / `test` / `clean`:

- `-c, --configuration` — `Debug` (default) or `Release`.
- `-j, --jobs` — max cells to run concurrently (default: all).
- `--fail-fast` — abort remaining cells on the first failure.
- `--derived-data` — DerivedData path (single cell) or base directory under which
  each parallel cell gets its own stable subdirectory.
- `--use-swb`, `--cache-hits`, `--swb-trace`, `--swb-trace-file` — Swift Build
  interception and tracing.

## Configuration

Configuration is a plist stored at `.cupertino/cupertino.lisp` in the project
root. Set it interactively with `cupertino init`, or non-interactively with
`cupertino config`:

```sh
cupertino config --scheme MyApp --sim ABC123        # default build scheme & simulator
cupertino config --schemes MyApp --schemes MyKit     # default schemes for a matrix
cupertino config --test-scheme MyAppTests            # default test scheme
cupertino config --use-swb true --cache-hits true    # Swift Build interception
cupertino config --max-jobs 8 --slow-threshold 5     # dashboard tuning
```

Run `cupertino config` with no arguments to view the current configuration.

## Development

Dependencies are managed with qlot (`qlfile` / `qlfile.lock`); the project-local
Quicklisp lives under `.qlot/`.

```sh
make deps     # qlot install
make test     # run the rove test suite via the project-local Quicklisp
make build    # build the binary
```

Tests are written with [rove](https://github.com/fukamachi/rove). You can also
run them from a REPL:

```lisp
(ql:quickload :cupertino/tests)
(rove:run-suite :cupertino/tests)
```

## License

[MIT](LICENSE) © Angel Peralta
