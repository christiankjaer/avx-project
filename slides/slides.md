# Data parallel Standard ML

By Christian Kjær Larsen (and Martin Elsman)

## Agenda

* Programming with vector instructions
* Programming with vectors in SML
* Value representations in functional languages
* Where do you add stuff in a compiler
* It's not really that hard
* Demo
* Beers

There will be a break somewhere in the middle

---

# Programming with vector instructions

SIMD: Single Instruction - Multiple Data

Packed SIMD - SIMD Within A Register (SWAR)

- Intel AVX
- ARM Neon

A register contains multiple values

---

## Low level

HARDCORE

---

## ¨Mid¨ level

Intrinsics

[C/C++](https://learn.microsoft.com/en-us/cpp/intrinsics/x86-intrinsics-list?view=msvc-170)

[Java Vector API](https://openjdk.org/jeps/426)

---

## High level

Auto vectorization (Fortan, clang, ...)

Futhark and friends

---

# Standard ML

Platonic ideal of a functional programming language

- Full type inference
- ADTs + Pattern matching
- Higher-order functions
- ~~Higher kinds and type classes~~
- Powerful module system

What are modules?

---

## Standard ML modules


# How to compile a functional language

1) Lexing, parsing, whatever

2) Type inference

3) Elaboration

4) Optimization

5) Code generation

# MLKit

# Design space

- Highly optimized C subroutines
  * FFI overhead
- Intrinsics

