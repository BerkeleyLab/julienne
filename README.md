<p align="center">
  <img width="250" height="250" src="https://github.com/user-attachments/assets/1a1c4f1a-f229-4d6d-bcaa-d5d9826ee639">
</p>

Julienne: Idiomatic Correctness Checking for Fortran 2023
=========================================================
The Julienne framework offers a unified approach to unit testing and checking 
runtime assertions.  Julienne defines idioms for specifying correctness
conditions in a common way when writing tests that wrap the tested procedures
or assertions that conditionally execute inside procedures.  Julienne's idioms
center around expressions built up from defined operations: a uniquely flexible
Fortran capability that allows developers to define _new_ operators in addition
to overloading the Fortran's intrinsic operators.  The following table provides
some examples of the expressions that can be written in assertions and tests
using Julienne:

Example expressions                               | Operand types              
--------------------------------------------------|--------------------------------------
`x .approximates. y .within. tolerance`           | `real`, `double precision`
`x .approximates. y .withinFraction. tolerance`   | `real`, `double precision`
`x .approximates. y .withinPercentage. tolerance` | `real`, `double precision`
`.all. ([i,j] .lessThan. k)`                      | `integer`, `real`, `double precision`
`(i .lessThan. j) .and. (k .equalsExpected. m))`  | `integer`, `real`, `double precision`
`x .lessThan. y`                                  | `integer`, `real`, `double precision`
`x .greaterThan. y`                               | `integer`, `real`, `double precision`
`i .greaterThan. j`                               | `integer`, `real`, `double precision`                 
`i .equalsExpected. j`                            | `integer`                 
`i .greaterThanOrEqualTo. j`                      | `integer`                 
`i .lessThanOrEqualTo. j`                         | `integer`                 

The operations and operands have the following properties:

1. The operand type and kind must be uniform throughout the expression.
2. All operations are `elemental`, which implies the operands must be conformable.
   Arrays of the same shape are conformable.  Scalars are conformable with arrays.

The above expressions produce automated diagnostic messages when an expression
is untrue.  Julienne also provides string-handling utilities for user customization
of diagnostic messages.  The following table shows some of the string expressions
that Julienne supports:

Example expressions                     | Result (`character`)
----------------------------------------|---------------------
.csv. [1,2,4]                           | "1,2,4"
s=string\_t("1,2,4"); s%bracket()       | "[1,2,4]"
s=string\_t("1,2,4"); s%bracket("{","}")| "{1,2,4}"
s=string\_t("1,2,4"); s%bracket("|")    | "|1,2,4|"

Assertions
----------
Functional programming patterns centered around `pure` procedures enhance
code clarity, ease refactoring, and encourage optimization.  For example,
the constraints on `pure` procedures make it easier for a developer or a
compiler to safely reorder program statements.  Moreover, Fortran allows
invoking only `pure` procedures inside `do concurrent`, a construct that
compilers can automatically offload to a graphics processing unit (GPU).

Julienne lowers a widely stated barrier to writing `pure` procedures (including
`simple` procedures): the difficulty in printing values while debugging code.
The Julienne philosophy is that printing a value for debugging purposes implies
an expectation about the value.  Assert such expectations by writing Julienne
expressions inspired by natural language.  A program will proceed quietly past
a correct assertion.  An incorrect assertion produces either automated or custom
diagnostic messages during error termination.

To write a Julienne assertion, insert a function-like preprocessor macro
`call_julienne_assert` on a single line as in the following program:
```fortran
#include "julienne-assertion-macros.h"
program main
  use, julienne_m, only : call_julienne_assert_
  implicit none
  real, parameter :: x=1., y=2., tolerance=3.
  call_julienne_assert(x .approximates. y .within. tolerance)
end program
```
where inserting `-DASSERTIONS` in a compile command will expand the macro to
```fortran
  call call_julienne_assert_(x .approximates. y .within. tolerance)
```
and where dots (`.`) delimit Julienne operators and the parenthetical expression
evaluates to a Julienne `test_diagnosis_t` object that has two components:

1. A `logical` indicator of the assertion's truth and
2. An automatically constructed diagnostic message.

Please see [Julienne operators] for a list of available operators.

Unit tests
----------
Writing tests using Julienne involves constructing a test-description array,
in which each element is a `test_description_t` constructor function invocation
with two arguments: a `character` string describing what the test does and the
name of a function that performs the test.  An example follows:
```fortran
  type(test_description_t), allocatable ::test_descriptions(:)
  test_descriptions = [ &
     test_description_t("checking something", check_something) &
    ,test_description_t("checking something else", check_something_else) &
  ]
```
Execute the tests by calling the `test_description_t` type-bound `run` procedure
on the `test_descriptions` array.

Define each test function to produce a `test_diagnosis_t` object result
encapsulating the two components enumerated in the [Assertions] section.
Use one of Julienne's expression idioms to construct the function result
automatically:
```fortran
  function check_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: i=1, j=1
    test_diagnosis = i .equalsExpected. j
  end function
```
where the `.equalsExpected.` operator is named to imply an asymmetry with
respect to the arguments `i` and `j`.  If the condition `i==j` evaluates to
`.false.`, Julienne constructs a diagnostic message reflecting the implied
asymmetry, i.e., indicating that `i` is the actual value, whereas `j` is the
expected value.

Please see the demonstration test suite in [demo README.md](./demo/README.md)
for more detailed instructions on setting up a new test suite, including how
to customize the diagnostic output using the string-manipulation capabilities
associated with Julienne's `string_t` derived type.  The demonstration test
suite's main program also shows how to use Julienne's `command_line_t` type
to access arguments that users pass to program via a command line or shell
script.  Julienne also offers useful input/output format strings and
format-generating functions.

An Origin Story
---------------
Julienne's name derives from the term for vegetables sliced into thin strings:
julienned vegetables.  The [Veggies] and [Garden] unit-testing frameworks
inspired the structure of Julienne's tests and output.  Initially developed in
the [Sourcery] repository as lightweight alternative with greater portability
across compilers, Julienne's chief innovation lies in the expressive idioms the
framework supports.

Getting Started
---------------
Please see the demonstration test suite in [demo README.md](./demo/README.md).

Compiler Support
----------------
The table below shows the compilers that Julienne fully or partially supports.
When built with a fully supported compiler, all Julienne tests pass.  When built
with a partially supported compiler, the Julienne test suite skips some tests
due to compiler bugs.  The test output reports which tests are skipped and
thereby details any features Julienne does not supported with a given compiler.

Compiler         | Version(s) Tested        | Known Issues
-----------------|--------------------------|-------------
LLVM `flang-new` | 19, 20                   | none
NAG `nagfor`     | 7.2 Build 7227           | none
GCC `gfortran`   | 13, 14, 15               | see 1 below
Intel `ifx`      | 2025.4 Build 20241205    | see 2 below

1. `gfortran` issues:
   - The `test_description_t` constructor's `diagnosis_function` actual argument
     must be a procedure pointer conforming to the `diagnosis_function_i`
     abstract interface.
   - The `string_t` `bracket` type-bound function crashes for GCC versions < 15.
   - Each element of a [`vector_test_description_t`] array (a feature to be
     deprecated in a future release) must be defined in a separate statement.
2. `ifx` issue:
   - Two `string_t` tests fail as described in issue [#51].

Building and Testing
--------------------

#### LLVM (`flang-new`) compiler
##### `flang-new` version 20 or later
```
fpm test --compiler flang-new
```

##### `flang-new` version 19
Add the following command before the `fpm` command recommended above for
LLVM 20 or later:
```
export FPM_FFLAGS="-mmlir -allow-assumed-rank"
```
where this `FPM_FFLAGS` setting turns on the support for Fortran's assumed-rank
dummy arguments.

If you do not have access to LLVM 19 or 20, we recommend building the
llvm-project main branch from source.  A script that might help with
doing so is in the [handy-dandy] repository.

### NAG (`nagfor`) compiler
```
fpm test --compiler nagfor --flag -fpp
```

#### GNU (`gfortran`) compiler
##### `gfortran` versions 14 or higher
```
fpm test --compiler gfortran --profile release
```

##### `gfortran` version 13
```
fpm test --compiler gfortran --profile release --flag "-ffree-line-length-none"
```
where the `-ffree-line-length-none` turns on support for lines exceeding the Fortran 2018 limit of 132 columns.
(Fortran 2023 expands the allowable line length to 5,000 characters.)

#### Intel (`ifx`) compiler
```
fpm test --compiler ifx --flag "-fpp -O3 -coarray" --profile release
```

Documentation
-------------
See our online [documentation] or build the documentation locally by installing [FORD] and executing `ford ford.md`.

[#51]: https://github.com/BerkeleyLab/julienne/issues/51
[Assertions]: #assertions
[`diagnosis_function_i`]: https://github.com/BerkeleyLab/julienne/blob/37bcc959efa8f9e27ae50fecfd37a6bf52ef0a43/src/julienne/julienne_test_description_m.f90#L16
[documentation]: https:///berkeleylab.github.io/julienne/
[FORD]: https://github.com/Fortran-FOSS-Programmers/ford
[Garden]: https://gitlab.com/everythingfunctional/garden
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/7caaa4dc3d6e5331914a3025f0cb1db5ac1a886f/src/fresh-llvm-build.sh
[Sourcery]: https://github.com/sourceryinstitute/sourcery
[`vector_test_diagnosis_i`]: https://github.com/BerkeleyLab/julienne/blob/37bcc959efa8f9e27ae50fecfd37a6bf52ef0a43/src/julienne/julienne_vector_test_description_m.F90#L18
[Veggies]: https://gitlab.com/everythingfunctional/veggies
