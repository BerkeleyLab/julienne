Pure Printing
=============

This directory contains a program and a supporting module/submodule
pair that collectively demonstrate the use of Julienne's
`stop_and_print` subroutine designed for use inside `pure` procedures.
Specifically, the program shows how to print the following entities:

- a text file,
- a two-dimensional (2D) integer array, or
- an object of derived type.

```
Usage:
  fpm run \
     --example pure-stop-and-print \
     --compiler flang --profile release \
     -- [-h|--help] | [--file <name>] | [--array] | [--derived-type]
```

where pipes (|) separate alternatives, square brackets ([]) delimit
optional arguments, and angular brackets (<>) delimit user input.

Getting help
------------
The following command prints the above usage text:

```
  fpm run --example pure-stop-and-print --compiler flang -- --help
```

Examples
--------
### Printing a 2D integer array
The following command prints a 2D integer array defined in the example program:

```
  fpm run --example pure-stop-and-print --compiler flang -- --array
```

### Printing a derived type
The following command prints the derived type defined in this directory's
[write_stuff_m](./write_stuff_m.F90)) module:

```
  fpm run --example pure-stop-and-print --compiler flang -- --derived-type
```

using the derived-type output procedure in the [write_stuff_s](./write_stuff_s.F90))
submodule.

### Printing a text file

The following command prints this repository's `fpm` mannifest: `fpm.toml`.

```
  fpm run --example pure-stop-and-print --compiler flang -- --file fpm.toml
```
