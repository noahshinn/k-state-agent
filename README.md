# KStateAgent

An experiment for state classification.

## Installation

Build from source:

```bash
git clone https://github.com/noahshinn/k-state-agent
```

run `make`

```bash
cd k-state-agent && make
```

## Data Generation

Data can be generated using the Rust data streaming client. `json` is the only format that is currently supported.

## Test run

Compile with `dune`

```bash
cd ./k_state_gen/bin/ && dune build @install
```

Run with `dune`:

```bash
dune exec k_state_gen
```

The output will be in `./example.json`
