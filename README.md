Julienne
========

Spun off from [Sourcery] and inspired by [Veggies], Julienne is a modern-Fortran unit-testing framework and utility for manipulating strings, including command lines and input/output format strings. 
This repository's name derives from the term for vegetables sliced into thin strings: julienned vegetables.
This software repository captures the authors' most frequently used thin slice of the Veggies and Sourcery repositories while avoiding certain compiler limitations of the other two repositories.
Julienne achieves portability across compilers through minimalism and isolation.
Thus Julienne has no external dependencies and offers limited but widely useful capabilities.
The string-handling and command-line parsing capabilities are included primarily because they support Julienne's unit-testing code.

Examples
--------
For examples of how to use Julienne, please see the [examples](./examples) subdirectory.

Building and Testing
--------------------
### GNU (`gfortran`) 13 or higher required
```
fpm test
```

### NAG (`nagfor`)
```
fpm test --compiler nagfor --flag -fpp
```

### LLVM Flang
```
FPM_FC=flang-new
FPM_FFLAGS="-mmlir -allow-assumed-rank"
fpm test # Warning: Do not override the FPM_FC or FPM_FFLAGS variables with --compiler or --flag
```
where the flags turn on LLVM Flang's experimental support for Fortran assumed-rank dummy arguments.
Flang's capabilities are evolving rapidly so we recommend building a recent version of the main branch of llvm-project.
A script that might be helpful for doing so is in the [handy-dandy] repository.

Documentation
-------------
See our online [documentation] or build the documentation locally by installing [FORD] and executing
```
ford ford.md
```
[Sourcery]: https://github.com/sourceryinstitute/sourcery
[Veggies]: https://gitlab.com/everythingfunctional/veggies
[here]: https://github.com/rouson/handy-dandy/blob/7caaa4dc3d6e5331914a3025f0cb1db5ac1a886f/src/fresh-llvm-build.sh
[documentation]: https://sourceryinstitute.github.io/julienne/
[FORD]: https://github.com/Fortran-FOSS-Programmers/ford 
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/7caaa4dc3d6e5331914a3025f0cb1db5ac1a886f/src/fresh-llvm-build.sh
