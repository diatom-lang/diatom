#!/bin/sh

dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd $dir

alias bench="hyperfine --warmup 1 --runs 3 --ignore-failure"
diatom="../target/release/diatom"

font="\033[1;31m"
normal="\033[0m"

# build in release mode
echo "${font}Building diatom...${normal}\n"
cargo build --release

# bench fibonacci
echo "\n${font}Benchmark fibonacci sequence...${normal}\n"
bench -n diatom "${diatom} fibonacci/fib.dm" -n lua "lua fibonacci/fib.lua" -n python "python3 fibonacci/fib.py"

# bench loop
echo "\n${font}Benchmark plain loop...${normal}\n"
bench -n diatom "${diatom} loop/loop.dm" -n lua "lua loop/loop.lua" -n python "python3 loop/loop.py"
