shellenv
------------------------------
Print evaluated variables in shell export instructons.

This allows sharing environment variables between shells.

Currently, only `bash` and `fish` syntaxes are supported.

Build & Install
------------------------------

```bash
# Copy template
install -bv env.template ~/.config/shellenv/env

# Copy binary
cargo build --release
install -bsv target/release/shellenv ~/.local/bin/

## Build in nightly channel
# cargo +nightly build -Z build-std=std --release --target x86_64-unknown-linux-gnu
# install -sv target/x86_64-unknown-linux-gnu/release/shellenv ~/.local/bin/
```

For Fish shell, add the following to `~/.config/fish/config.fish`
```
$HOME/.local/bin/shellenv --format fish $HOME/.config/shellenv/env | source
```
For Bash shell, add the following to `~/.bashrc`
```
source <("${HOME}/.local/bin/shellenv" --format bash "${HOME}/.config/shellenv/env")
```
For Dash shell, use `eval "$(...))` to source the output command (untested).

See `shellenv --help` for available options.

Parameter Expansion
------------------------------

Basic parameter expansion is supported:

`${VAR}`, `${VAR:-default}`, `${VAR:+alt}`, and their counterparts without colons.

Nested expansion is also allowed in value: `${VAR:-${VAR2}}`.

But expansion in parameter name (i.e. `${${NAME}:-default}`) is not supported.

Also, `~` is not expanded; use `${HOME}` instead.

Others
--------

**Afterword**


The compiled binary is big (~1MiB after strip).
The `clap` crate is roughly 50% the cause.
See `cargo bloat` for proportions.