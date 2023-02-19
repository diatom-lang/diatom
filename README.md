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

A dynamic typed scripting language for embedded use in applications. This project is yet another attempt of being a "better" lua.

**Warning**: Project is still in experimental stage and API is considered as unstable.

## Features
- [x] **Never panic**\*
- [x] **High Performance**\*\*: Runs at most **2x** speed of lua 5.4, **6x** speed of python 3.10
- [ ] Support for a tracing jit (Planned) 
- [x] **rustc style** helpful **error message**
- [x] No global variable
- [x] No `nil/None` value
- [x] Has real integer type 
- [x] Has **0-indexed** real **list** type
- [x] Support **tuple** for multiple return
- [x] Support for string indexed **table**
- [x] Support for **meta table** and **OOP style method call syntax**
- [ ] Support for gradual typing (Planned)
- [ ] Support for macro system (Planned)

\*: Panic may still be triggered by external functions  
\*\*: Results are from our benchmarks which may vary betweens different builds and test codes.

 
## Quick Start

#### Try Diatom online
You can try diatom at [the online playground](https://diatom-lang.github.io/diatom-playground/).

#### Install with cargo
Make sure you have [Rust and Cargo](https://doc.rust-lang.org/book/ch01-01-installation.html) installed.
```sh
cargo install diatom
diatom --help # show help for diatom CLI
diatom # Enter diatom REPL console
```

#### Build from source
Run the following script:
```sh
git clone https://github.com/diatom-lang/diatom.git
cd diatom
cargo run --release # Run interactive console
```

#### Syntax highlight
- Vim/Neovim plugin: [diatom.vim](https://github.com/diatom-lang/diatom.vim)


## Reference & Grammar

#### The Reference Book
The Diatom Reference (with examples) is available at [here](https://diatom-lang.github.io/reference/). 

#### Examples
For standard library function and built-in types and methods checks [examples](examples/). These examples are checked by unit tests and will never out of date.

## Embedding in Application

#### Embedding in Rust Application

Diatom is available at [crates.io](https://crates.io/crates/diatom) with detailed examples.

To use latest build, add this repo to your `Cargo.toml`. Latest build documentation is available [here](https://diatom-lang.github.io/diatom).

#### Use C bindings

Work in progress.

## Feature Showcase

#### Functional style std-lib
<img width="890" alt="Screenshot 2023-02-19 at 10 23 38 PM" src="https://user-images.githubusercontent.com/63455223/219954981-47d4e2c9-cbac-4f5d-af07-43556ed36ec9.png">

#### Pretty error reporting and trace-back
<img width="885" alt="Screenshot 2023-02-19 at 10 00 53 PM" src="https://user-images.githubusercontent.com/63455223/219954955-1c6b4e10-2549-4184-a39a-70f71fcde29a.png">

#### Expression based syntax
<img width="889" alt="Screenshot 2023-02-19 at 10 25 16 PM" src="https://user-images.githubusercontent.com/63455223/219954993-c5d4f493-3d40-474e-a777-27d2f207511e.png">

#### Interactive REPL console with **syntax highlight**
<img width="890" alt="Screenshot 2023-02-19 at 10 26 39 PM" src="https://user-images.githubusercontent.com/63455223/219955007-f2ebaebc-1265-4cf1-95f1-297825e5fed7.png">

