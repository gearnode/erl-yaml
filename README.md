# Project
This repository contains an implementation of the [YAML](https://yaml.org)
data format in Erlang.

Low-level parsing is based on [libyaml](https://github.com/yaml/libyaml) and
uses a [NIF](https://erlang.org/doc/man/erl_nif.html). Care has been taken to
use
[`enif_schedule_nif`](https://erlang.org/doc/man/erl_nif.html#enif_schedule_nif)
and
[`enif_consume_timeslice`](https://erlang.org/doc/man/erl_nif.html#enif_consume_timeslice)
to avoid blocking the scheduler during parsing.

Serialization uses the libyaml emitter API. It is currently not configurable.

# Documentation
A handbook is available [in the `doc`
directory](https://github.com/exograd/erl-yaml/blob/master/doc/handbook.md).

# Contact
If you find a bug or have any question, feel free to open a GitHub issue or to
contact me [by email](mailto:khaelin@gmail.com).

Please note that I do not currently review or accept any contribution.
