Bran
====

[![Circle CI](https://circleci.com/gh/szktty/bran.png?style=badge)](https://circleci.com/gh/szktty/bran)

A strongly-typed language with type inference running on Erlang VM, influenced by OCaml.


## Requirements

### For usage

- Erlang/OTP 17.3

### For build

- OCaml 4.02.1
- Menhir 20140422
- Spotlib 2.5.0
- OMake 0.9.8.6-0.rc1

## For test

- OUnit2 2.0.0


## Building from source

```
$ omake
```

## Installation

1. Execute `make` at `liberl` directory to compile Erlang sources used by compiled Bran modules.

  ```
  $ cd liberl
  $ make
  ```

2. Copy this directory or create a symbolic link to installation destination.

  ```
  $ ln -s /home/yourname/bran /opt/local/bran
  ```

3. Add path of the `bin` directory to command line path. (environment variable `PATH`, etc.)

  ```
  # .bashrc, etc.
  export PATH=/opt/local/bran/bin:$PATH
  ```

4. Set environment variable `BRAN_LIBS` to the `lib` directory including signature files of Bran.

  ```
  # .bashrc, etc.
  export BRAN_LIBS=/opt/local/bran/lib
  ```

5. Add path of the `liberl/ebin` directory, including Erlang modules, to environment variable `ERL_LIBS`.

  ```
  # .bashrc, etc.
  export ERL_LIBS=/opt/local/bran/liberl/ebin:$ERL_LIBS
  ```

6. OK, let's try `bran` command.

  ```
  $ bran
  ```

## Usage

```
# compile
$ ./bran fib.br

# use as Erlang module
$ erl
...
1> fib:fib(10).
89
```

## ToDo

- Data types
  - Map
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
- float
- char
- string
- list (empty lists must be specified type. for example: `([] : int list)`)
- bitstring

### Comment

```
# comment
```

### Literals

#### Atom

```
@atom
@Atom
@atom_ok
@Atom_ok
@"atom"
@"a t o m"
@"*a+t-o/m!?* :-@"
```

#### String

- On Bran, String is **not** a list of character type data. `string` and `char list` are different types.
- Escape sequences are the same as one of Erlang.

```
"string"
"42"
"hello, world!\n"
"\052"
"\x00\xff"
```

#### Character

- Character literals are single quote characters.

```
'0'
'a'
'Z'
'\n'
'\052'
'\xff'
```

#### Bitstring

Basically bitstring syntax is the same as one of Erlang. See [Bit Syntax Expressions](http://www.erlang.org/doc/reference_manual/expressions.html#bit_syntax).

```
<<42>>
<<"abc">>
<<1,17,42:16>>
<<123/int-native>>
<<123/unsigned-big-integer>>
<"abc"/utf8>>
```

#### Integer

- Erlang's `base#value` is `base r value` (ex. 16rff). Because `#` is used by comment in Bran.
- Integer has any number of digits.

```
42
042 # -> 42
2r101
16r1f
```

#### Float

```
2.3
2.3e3
2.3e-3
7.000e+00
```

#### List

```
[]
[1]
[1, 2, 3]
[1, 2, 3,]
```

#### Tuple

```
(1, "a")
```

#### Array

```
[||]
[|1|]
[|1, 2, 3|]
[|1, 2, 3,|]
```

### Variable bindings

```
var x = 1
```

### Function

#### Definition

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

#### Call

```
f x y         # Call function "f" with "x" and "y" arguments
f x $ g y z   # => f x (g y z)  Haskell's "$"
```


### External functions

`.bri`:

```
external print_bool : bool -> unit = "bran_lib_pervasives:print_bool"
```

### Calling Erlang functions

use `Erlang.eval : string -> string`.

```
var result = Erlang.eval "ok."
```

### Conditional

Conditional expression syntax is:

```
if [bool-exp] then [block] end
if [bool-exp] then [block] else [block] end
if [bool-exp] then [expr] else [expr]
```

"`end`" can be omitted when blocks have only an expression.

Examples:

```
if x then
  print_string "true"
  true
end

if x then
  print_string "true"
  true
else
  print_string "false"
  false
end

if x then true else false

if x then true
else false

if x then
  true
else
  false
```

### Monad

```
perform
  x1 <- action1
  x2 <- action2
  action3 x1 x2
  return x1
end
```
