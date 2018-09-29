# re:compile

This is yet another reloader for Erlang.

# Usage

Just start the `recompile` application as part of a shell or (development) release.
`dev_mode` needs to be set to `true` for release.

For more examples, refer to [recopile\_example](https://github.com/bullno1/recompile_example).

To use to polling mode, set the app variable `scan_interval` to a value other than `infinity`.
The recommneded value is `1000`.

# Features

* Packaged as an application rather than a plugin so that it can work under [rebar3\_run](https://github.com/tsloughter/rebar3_run).
* Invokes `rebar3` directly and thus, supports any plugins that generates beam code such as `rebar3_gpb_plugin` or `rebar3_erlydtl_plugin`.
* Optional polling mode for systems such as Vagrant
