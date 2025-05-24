# larena

# High-Performance Arena Allocator (x86-64 ASM)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://github.com/abiiranathan/larena/actions/workflows/build.yml/badge.svg)](https://github.com/abiiranathan/larena/actions)

A zero-dependency, hand-optimized arena allocator written in x86-64 assembly with Intel syntax. Designed for maximum performance in memory-intensive applications.

## Features

- ⚡ **Blazing Fast** - Hand-tuned assembly with AVX optimizations
- 🧠 **Smart Prefetching** - Adaptive caching strategies
- � **Tiny Footprint** - <500 bytes of code
- 🚫 **No Dependencies** - Pure ASM implementation
- 🛡️ **Relatively Memory Safe** - Bounds-checked allocations

## Performance
20 - 100x faster compared to malloc depending on the size of the allocation.


## Installation

```bash
# Clone with submodules
git clone --recursive https://github.com/abiiranathan/larena.git
cd larena

make

sudo make install
```