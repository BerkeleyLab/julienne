Julienne
========

Julienne is a modern-Fortran unit-testing framework that includes utilities for manipulating strings, including command lines and input/output format strings.
Users construct tests by extending Julienne's `abstract` `test_t` derived type and constructing an array of `test_description_t` objects, each of which encapsulates a description string and the name of a function that produces one or more `test_diagnosis_t` objects, each of which in turn encapsulates a `logical` test outcome and a `diagnostics_string`.
Julienne empowers users to determine the diagnostic information printed when tests fail.
To support customized diagnostics output, Julienne provides a `string_t` derived type with corresponding constructor functions and useful operators.

Julienne's name derives from the term for vegetables sliced into thin strings: julienned vegetables.
The structure of Julienne's tests and output was inspired by the [Veggies] unit-testing framework with the aim of being more lightweight and portable across compilers and compiler versions.

Getting Started
---------------
Please see the [example-test-suite README.md](./example/example-test-suite/README.md).

Supported Compilers 
-------------------

Compiler         | Version(s) Tested        | Known Issues
-----------------|--------------------------|-------------
LLVM `flang-new` | 19, 20                   | none
Intel `ifx`      | 2025.1                   | none
NAG `nagfor`     | 7.2 Build 7227           | none
GCC `gfortran`   | 14.1.0, 14.2.0_1, 15.0.1 | 1-3 below

Compiler bugs related to the following issues have been reported to the GCC project:

1. The `test_description_t` constructor's `diagnosis_function` actual argument must be a procedure pointer.
2. Each element of a `vector_test_description_t` array must be defined in a separate program statement.
3. If executed, the `string_t` type's `bracket` procedure causes a program crash.

Some related `gfortran` fixes have already been developed and pushed to the GCC branches for release with GCC 14.3 and 15.1.0.

Building and Testing
--------------------

### GNU (`gfortran`)
#### `gfortran` versions 14 or higher
```
fpm test --compiler gfortran --profile release
```

#### `gfortran` version 13
```
fpm test --compiler gfortran --profile release --flag "-ffree-line-length-none"
```
where the `-ffree-line-length-none` turns on support for lines exceeding the Fortran 2018 limit of 132 columns.
(Fortran 2023 expands the allowable line length to 5,000 characters.)

### Intel (`ifx`) 2025.4 Build 20241205 tested
```
fpm test --compiler ifx --flag "-fpp -O3 -coarray" --profile release
```

### LLVM (`flang-new`)
#### `flang-new` version 20 or later
```
export FPM_FC=flang-new
fpm test
```

#### `flang-new` version 19
Add the following command before the `fpm` command recommended above for LLVM 20 or later:
```
export FPM_FFLAGS="-mmlir -allow-assumed-rank"
```
where this `FPM_FFLAGS` setting turns on the support for Fortran's assumed-rank dummy arguments.

If you do not have access to LLVM 19 or 20, we recommend building the main branch of llvm-project from source.
A script that might be helpful for doing so is in the [handy-dandy] repository.

### NAG (`nagfor`)
```
fpm test --compiler nagfor --flag -fpp
```

Documentation
-------------
See our online [documentation] or build the documentation locally by installing [FORD] and executing `ford ford.md`.

[Sourcery]: https://github.com/sourceryinstitute/sourcery
[Veggies]: https://gitlab.com/everythingfunctional/veggies
[here]: https://github.com/rouson/handy-dandy/blob/7caaa4dc3d6e5331914a3025f0cb1db5ac1a886f/src/fresh-llvm-build.sh
[documentation]: https:///berkeleylab.github.io/julienne/
[FORD]: https://github.com/Fortran-FOSS-Programmers/ford 
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/7caaa4dc3d6e5331914a3025f0cb1db5ac1a886f/src/fresh-llvm-build.sh
