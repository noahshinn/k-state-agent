# k-state-agent
A model-free agent trained to act optimally over an unsupervised-generated temporal state space written in Ocaml and Rust.

## Implemented
  - data streaming + capturing
  - state normalization
    - enforces absolute and relative magnitude invariances
  - clustering methods
    - k means (baseline)

## in dev
  - clustering methods
    - dynamic time warping (to account for translation)
    - soft dynamic time warping
  - deep q-learning to act on state space
