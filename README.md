# The Diatom Programming Language
![Unit Tests](https://github.com/diatom-lang/diatom/actions/workflows/rust.yml/badge.svg)
![doc](https://github.com/diatom-lang/diatom/actions/workflows/rustdoc.yml/badge.svg)
[![Crates.io][crates-badge]][crates-url]
[![license][license-badge]][crates-url]

[![dependency status](https://deps.rs/repo/github/diatom-lang/diatom/status.svg)](https://deps.rs/repo/github/diatom-lang/diatom)
![issue](https://img.shields.io/github/issues/diatom-lang/diatom)
![pr](https://img.shields.io/github/issues-pr/diatom-lang/diatom)
![coverage](https://img.shields.io/codecov/c/github/diatom-lang/diatom)

[crates-badge]: https://img.shields.io/crates/v/diatom.svg
[crates-url]: https://crates.io/crates/diatom
[license-badge]: https://img.shields.io/crates/l/diatom

A dynamic typed scripting language for embedded use in applications.

## Warning: Project is under development and not useable now.

## Features
 - [ ] **Gradual Typing**: Support for optional type hint.
 - [x] **Security**: Diatom is completely isolated and is only able to interact with its host.
 - [x] **ADT**: Diatom use algebraic data type.
 - [x] **No Null, No Exceptions**: Diatom encourages the use of `Maybe` **functor** and **monad**. The concept of `null` and `exception` in other languages like Java is completely useless in Diatom.
 - [x] **Impure Functional**: Diatom makes everything mutable and public like Python. We also support a little OOP in `data`(aka `class` in OOP languages) declaration.
 - [ ] **Native Speed**: Given every variable is marked, diatom is able to theoretically reach C/C++ level speed.

## Try Diatom

You can try diatom at [the online playground](https://diatom-lang.github.io/diatom-playground/).
 
## Quick Start

Make sure you have [Rust and Cargo](https://doc.rust-lang.org/book/ch01-01-installation.html) installed.
```sh
cargo install diatom
diatom # Run interactive console
```
## Installing from source
Run the following script:
```sh
git clone https://github.com/diatom-lang/diatom.git
cd diatom
cargo run --release # Run interactive console
```

## Reference

The Diatom Reference is available at [here](https://diatom-lang.github.io/reference/).

## Embedding in Application

### Rust Application

Diatom is available at [crates.io](https://crates.io/crates/diatom).

To use latest build, add this repo to your `Cargo.toml`. Latest build documentation is available [here](https://diatom-lang.github.io/diatom).

### Use C bindings

Work in progress.


