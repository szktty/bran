Bran
====

A strongly-typed language with type inference running on Erlang VM, influenced by OCaml.


## Requirements

- OCaml 4.02.1
- Menhir 20140422
- Spotlib 2.5.0
- OMake 0.9.8.6-0.rc1


## Build

```
$ omake
```

## Usage

```
# at "src" directory
$ ./bran fib.br

# add internal library directory to library path (-pa)
$ erl -pa erl
...
1> fib:fib(10).
89
```

## ToDo

- Data types
  - Character
  - Float
  - Option
  - Reference
  - Exception
  - Binary
  - Process
- Type system
  - Records
  - Variants
  - Polymorphic type
- Syntax
  - Pattern matching
  - Exception handling
  - Labeled arguments and optional arguments
  - Message passing
- Compilation
  - Optimization
  - Executable file generation (escript)
- Library
  - Standard library
  - `Obj` module
  - OTP
- Tools
  - Interactive shell
  - Source browsing support
  - Build tool support (rebar)
- Build
  - Installation
  - Using packaging tools (OPAM, etc.)

## Syntax

### Files

- `.bri` (interface file)
- `.br` (implementation file)

### Data Types

- unit
- bool
- int
- string
- list (empty lists must be specified type. for example: `([] : int list)`)

### Comment

```
# comment
```

### Variable bindings

```
var x = 1
```

### Functions

```
def f x y = x + y

def [rec] f x ... =
  [exp]
  ...

```

signature (`.bri`):

```
def f : int -> int -> int
```

### External functions

`.bri`:

```
external print_bool : bool -> unit = "bran_lib_pervasives:print_bool"
```

### Conditional

```
if [exp] then
  ...
else
  ...
end
```

### Calling Erlang functions

use `Erlang.eval : string -> string`.

```
var result = Erlang.eval "ok."
```

