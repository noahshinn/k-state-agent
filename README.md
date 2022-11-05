# KStateAgent 
A model-free agent trained to act optimally over an unsupervised-generated temporal state-space written in Ocaml and Rust.

## Terminology
A `structure` is a multi-dimensional object that captures a particular time-series.

A `state` is a `structure` that captures a particular region of time-series structure similarity.

## Installation
Build from source:
```
git clone https://github.com/noahshinn024/k-state-agent
```
run `make`
```
cd k-state-agent && make
```

## Data Generation
`json` is the only format that is currently supported. Data can be generated using the Rust data streaming client.

## State Generation
KStateAgent supports the following state generation strategies:
  - scaled and normalized k means (using euclidean distance over `structures`)

#### Test run

Compile with `dune`
  ```
cd ./k_state_gen/bin/ && dune build @install
```
Run with `dune`:
```
dune exec k_state_gen
```
The output will be in `./example.json`

In dev:
  - dynamic time warping (DTW) to establish translational invariance
  - soft dynamic time warping (soft-DTW) to account for non-differentiable regions

## Agent Training
KStateAgent is aimed to be a model-free agent trained via Q-learning or deep Q-Learning (if needed)

Implemented:
  - Q-learning utils

In dev:
  - training loop
