Tests
=====

## Requirements

- OUnit 2.0.0


## Build

```
$ omake
```


## Usage

```
$ ./run

# Show list of suites
$ ./run -list-test
...
0:parsing:0:simple_def
...

# Select tests
$ ./run -only-test 0:parsing:0:simple_def
```

For more detail, see `./run -help`.
