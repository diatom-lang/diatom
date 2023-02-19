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
The Diatom Reference is available at [here](https://diatom-lang.github.io/reference/) (out-dated currently). 

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
<img width="547" alt="Screenshot 2023-02-19 at 5 26 45 PM" src="https://user-images.githubusercontent.com/63455223/219940116-b313caed-f86b-474d-94fc-2618d196377e.png">

#### Pretty error reporting and trace-back
<img width="762" alt="Screenshot 2023-02-19 at 5 26 17 PM" src="https://user-images.githubusercontent.com/63455223/219940145-044e9094-f840-4929-a5b2-0405955bf4d4.png">

#### Expression based syntax
<img width="792" alt="Screenshot 2023-02-19 at 5 29 19 PM" src="https://user-images.githubusercontent.com/63455223/219940155-f5295365-869a-41d9-826d-279ca4b7b0a0.png">

#### Interactive REPL console with **syntax highlight**
 <img width="757" alt="Screenshot 2023-02-19 at 5 35 11 PM" src="https://user-images.githubusercontent.com/63455223/219940243-9572869b-5089-464e-a233-4dca3ea306a2.png">


