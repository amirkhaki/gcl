# gcl

A parser for a Guarded Command Language (GCL) written in Haskell.

## Description

This project provides a parser for a simple Guarded Command Language. The language supports:
- Basic arithmetic expressions
- Boolean expressions
- Relational operators (`=` and `>`)
- Alternative statements (`if ... fi`)
- Repetitive statements (`do ... od`)
- Assignments (`:=`)

## Building and Running

### Local Development

You can build and run the project using `cabal`:

```bash
cabal run gcl -- sample.gcl
```

Alternatively, you can use [Nix](https://nixos.org/) flakes:

```bash
nix run . -- sample.gcl
```

### Running without Cloning

You can also run the project directly from GitHub without cloning the repository.

First, create a file named `sample.gcl` with the following content:
```gcl
a := 2;
b := 3;
if
   (a > 2) -> c := 3; b := 4; a := 2
[] false -> c := 4
[] true -> c := 5
fi
;
do
   (a > 2) -> c := 3
[] false -> c := 4
[] true -> c := 5
od;
d := 4
```

Then, run the following command:
```bash
nix run github:amirkhaki/gcl -- sample.gcl
```

## GCL Syntax

Here is an example of the GCL syntax that can be parsed by this program:

```gcl
a := 2;
b := 3;
if
   (a > 2) -> c := 3; b := 4; a := 2
[] false -> c := 4
[] true -> c := 5
fi
;
do
   (a > 2) -> c := 3
[] false -> c := 4
[] true -> c := 5
od;
d := 4
```