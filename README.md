# Rust generator benchmark

Benchmarking different Rust generator libraries in a real world use case.

## Tested libraries

Among the benchmarked libraries are many of the most popular generator libraries for Rust, as well as anything I could find from [crates.io](https://crates.io/) with the [#generator](https://crates.io/keywords/generator) tag. These are compared against Rust's native (unstable) generators as well as alternative ways to implement the same test case without generators.

#### [next-gen](https://crates.io/crates/next-gen) 0.0.10 and 0.1.1

This library implements generators using Rust's async/await mechanism. It can create normal and boxed generators. Normal generators are faster but boxed generators can be moved (e.g. returned from a function) whereas normal generators can not.

#### [genawaiter](https://crates.io/crates/genawaiter) 0.99.1

A similar library also using async/await. It can create stack, rc and sync variants of generators. Stack is the fastest, rc can be moved and sync can be used simultaneously from multiple threads.

#### [generator-rs](https://crates.io/crates/generator) 0.7.0

This is an older library that implements stackful generators. It can create normal and local generators, but I don't know how they differ.

#### [corosensei](https://crates.io/crates/corosensei) 0.1.2

This brand new library also implements stackful generators.

#### [gen-z](https://crates.io/crates/gen-z) 0.1.0

This is a bit different from the others in that it implements async generators, meaning that async functions can be awaited inside the generator.

#### [Rust native generators](https://doc.rust-lang.org/beta/unstable-book/language-features/generators.html)

Rust's native implementation of generators is benchmarked too. The implementation is currently unstable and requires nightly Rust to compile.

#### Alternatives

For comparison, there are also non-generator versions of the same test case, one using iterators and another using just loops. The loop version can not be used to replace generators in most cases, and is included only as a point of reference.

## Test setup

**CPU:** Intel Core i5-4570

**OS:** Ubuntu 20.04

**Rust version:** 1.62.0-nightly (8bf93e9b6 2022-04-09)

**Benchmarking tool:** Criterion 0.3.5

#### Test scenario

The test scenario used is a real world use case for generators. It is a move generator taken from my [Battle Sheep board game AI](https://github.com/Tuupertunut/BattleSheepSolver). It generates all the moves that a player can play on one turn in Battle Sheep. It is very analogous to a [move generator](https://www.chessprogramming.org/Move_Generation) in chess AIs.

## Results

![violin plot of results](report/generator-benchmark/report/violin.svg)

| Implementation                   | Time\*    |
| -------------------------------- | --------- |
| no generator, just loops         | 1.5417 μs |
| no generator, just iterators     | 1.7546 μs |
| native rust generator (unstable) | 1.6797 μs |
| next-gen 0.1.1                   | 3.4451 μs |
| next-gen 0.1.1 boxed             | 3.4658 μs |
| next-gen 0.0.10                  | 2.4445 μs |
| next-gen 0.0.10 boxed            | 2.4403 μs |
| genawaiter stack                 | 2.8838 μs |
| genawaiter rc                    | 3.1552 μs |
| genawaiter sync                  | 7.8809 μs |
| generator-rs local               | 9.7056 μs |
| generator-rs                     | 9.7822 μs |
| corosensei                       | 6.1294 μs |
| gen-z                            | 20.874 μs |

\*_Time_ means the time taken to generate all moves for one Battle Sheep board state.

[Full report and numbers here](https://tuupertunut.github.io/rust-generator-benchmark/report/generator-benchmark/report/index.html)

The most interesting result here is that the fastest stable generator implementation in Rust is an old version of next-gen! The newer version of next-gen is much slower and falls behind genawaiter. Being movable (so next-gen boxed and genawaiter rc) does not seem to affect performance much compared to non-movable generators.

As expected, the Rust native implementation is faster than any library. It is notable that it is almost a _zero cost abstraction_, being very close in performance to not using a generator at all.

The stackful generators and the asynchronous gen-z suffer from having additional features that only slow them down in this simple test case. Out of the stackful generators it seems that corosensei is faster, although the stackfulness is not really used here.

## How to run these benchmarks myself?

- Clone this repository
- Install nightly Rust
- Run create-report.sh
