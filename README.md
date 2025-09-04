<p align="center">
  <img width="250" height="250" src="https://github.com/user-attachments/assets/1a1c4f1a-f229-4d6d-bcaa-d5d9826ee639">
</p>

Julienne: Idiomatic Correctness Checking for Fortran 2023
=========================================================
The Julienne framework offers a unified approach to unit testing and runtime
assertion checking.  Julienne defines idioms for specifying correctness
conditions in a common way when writing tests that wrap the tested procedures
or assertions that conditionally execute inside procedures to check correctness.
Julienne's idioms center around expressions built from defined operations: a
uniquely flexible Fortran capability allowing developers to define _new_
operators in addition to overloading Fortran's intrinsic operators.  The
following table provides examples of the expressions Julienne supports:

Example expressions                               | Operand types
--------------------------------------------------|-----------------------------------------------------------
`x .approximates. y .within. tolerance`           | `real`, `double precision`
`x .approximates. y .withinFraction. tolerance`   | `real`, `double precision`
`x .approximates. y .withinPercentage. tolerance` | `real`, `double precision`
`.all. ([i,j] .lessThan. k)`                      | `integer`, `real`, `double precision`
`.all. ([i,j] .lessThan. [k,m])`                  | `integer`, `real`, `double precision`
`.all. (i .lessThan. [k,m])`                      | `integer`, `real`, `double precision`
`(i .lessThan. j) .also. (k .equalsExpected. m))` | `integer`, `real`, `double precision`
`x .lessThan. y`                                  | `integer`, `real`, `double precision`
`x .greaterThan. y`                               | `integer`, `real`, `double precision`
`i .equalsExpected. j`                            | `integer`, `character`, `type(c_ptr)`
`i .isAtLeast. j`                                 | `integer`, `real`, `double precision`
`i .isAtMost. j`                                  | `integer`, `real`, `double precision`
`s .isBefore. t`                                  | `character`
`s .isAfter. t`                                   | `character`
`.expect. command_line%argument_present("--help")`| `logical`

where 
* `.isAtLeast.` and `.isAtMost.` can alternatively be spelled `.greaterThanOrEqualTo.` and `.lessThanOrEqualTo.`, respectively;
* `.equalsExpected.` uses `==`, which implies that trailing blank spaces are ignored in character operands;  
* `.equalsExpected.` with integer operands supports default integers and `integer(c_size_t)`;
* `.isBefore.` and `.isAfter.` verify alphabetical and reverse-alphabetical  order, respectively; and
* `.all.` aggregates arrays of expression results, reports a consensus result, passes, and shows diagnostics only for failing tests, if any;
* `.equalsExpected.` generates asymmetric diagnostic output for failures, denoting the left- and right-hand sides as the actual value and expected values, respectively; and.
* appending a trailing string to an idiom with `operator(//)` appends the string to the resulting diagnostics string, if any.

Expressive idioms 
-----------------
### Assertions
Any of the above expressions can be the actual argument in an invocation
of Julienne's `call_julienne_assert` function-line preprocessor macro:
```fortran
call_julienne_assert(x .lessThan. y)
```
which a preprocessor will replace with a call to Julienne's assertion subroutine
when compiling with `-DASSERTIONS`.  Otherwise, the preprocessor will remove the
above line entirely when `-DASSERTIONS` is not present.

### Unit tests
The above tabulated expressions can also serve as function results in unit tests.

### Constraints
All operands in an expression must be compatible in type and kind as well as
conformable in rank, where the latter condition implies that the operands must
be all scalars or all arrays with the same shape or a combination of scalars and
arrays with the same shape. This constraint follows from each of the binary
operators being `elemental`.  The unary `.all.` operator applies to operands of
any rank.

Each tabulated expression above produces a `test_diagnosis_t` object with two
components:

- a `logical` indicator of test success if `.true`. or failure if `.false.` and
- an automated diagnostic messages generated only if the test or assertion fails.

Custom Test Diagnostics
-----------------------
For cases in which Julienne's operators do not support the desired correctness
condition, the framework provides string-handling utilities for use in crafting
custom diagnostic messages.  The string utilities center around Julienne's
`string_t` derived type, which offers `elemental` constructor functions, i.e., 
functions that one invokes via the same name as the derived type: `string_t()`.
The `string_t()` constructor functions convert data of numeric type to
`character` type, storing the resulting `character` representation in a private
component of the constructor function result.  The actual argument provided to
the constructor function can be of any one of several types, kinds, and ranks.

Julienne provides defined operations for concatenating `string_t` objects
(`//`), forming a concatenated `string_t` object from an array of `string_t`
objects (`.cat.`), forming a separated-value list (`.separatedBy.` or
equivalently `.sv.`), including a comma-separated value list `(.csv.)`.  The
table below shows some expressions that Julienne supports with these defined
operations.

Example expression                               | Result
-------------------------------------------------|------------------------------------------------
`s%bracket()`, where `s=string_t("abc")`,        | `string_t("[abc]")`
`s%bracket("_")`, where `s=string_t("abc")`      | `string_t("_abc_")`
`s%bracket("{","}")`, where `s=string_t("abc")`  | `string_t("{abc}")`
`string_t(["a", "b", "c"])`                      | `[string_t("a"), string_t("b"), string_t("c")]`
`.cat. string_t([9,8,7])`                        | `string_t("987")`
`.csv. string_t([1.5,2.0,3.25])`                 | `string_t("1.50000000,2.00000000,3.25000000")`
`"-" .separatedBy. string_t([1,2,4])`            | `string_t("1-2-4")`
`string_t("ab") // string_t("cd")`               | `string_t("abcd")`
`"ab" // string_t("cd")`                         | `string_t("abcd")`
`string_t("ab") // "cd"`                         | `string_t("abcd")`

One can use such expressions to craft a diagnostic message when constructing
a custom test function result:
```fortran
type(test_diagnosis_t) test_diagnosis
test_diagnosis = test_diagnosis_t( &
  test_passed = i==j, &
  diagnostics_string = "expected " // string_t(i) // "; actual " //string_t(j) &
)
```

A file abstraction
------------------
Arrays of `string_t` objects provide a convenient way to store a ragged-length
array of `character` data.  Julienne's `file_t` derived type has a private
component that is a `string_t` array, wherein each element is one line of a text
file. By storing a file in a `file_t` object using the `file_t` derived type's
constructor function one can confine a program's file input/output (I/O) to one
or two procedures.  The resulting `file_t` object can be manipulated elsewhere
without incurring the costs associated with file I/O.  For example, the following
line reads a file named `data.txt` into a `file_t` object and associates the name
`file` with the resulting object.
```fortran
type(file_t) file
associate(file => file_t("data.txt"))
end associate
```
This style supports functional programming patterns in two ways. First, the rest
of the program can be comprised of `pure` procedures, which are precluded from
performing I/O.  Second, an associate name is immutable when associated with an
expression, including an expression that is simply a function reference. 
Functional programming revolves around creating and using immutable state.
(By contrast, when associating a name with a variable or array instead of with
an expression, only certain attributes, such as the entity's allocation status,
are immutable. The value of such a variable or array can be redefined.)

Functional Programming 
----------------------
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

Getting Started
---------------
### Writing Unit Tests
Please see [demo/README.md](./demo/README.md) for a detailed demonstration of
test setup.  

### Writing Assertions
To write a Julienne assertion, insert a function-like preprocessor macro
`call_julienne_assert` on a single line as in each of the two macro
invocations below:
```fortran
#include "julienne-assertion-macros.h"
program main
  use, julienne_m, only : call_julienne_assert_
  implicit none
  real, parameter :: x=1., y=2., tolerance=3.
  call_julienne_assert(x .approximates. y .within. tolerance)
  call_julienne_assert(abs(x-y) < tolerance)
end program
```
where inserting `-DASSERTIONS` in a compile command will expand the macros to
```fortran
  call call_julienne_assert_(x .approximates. y .within. tolerance, __FILE__, __LINE__)
  call call_julienne_assert_(allocated(a), __FILE__, __LINE__)
```
and where dots (`.`) delimit Julienne operators.  The above expression containing
Julienne operators evaluates to a Julienne `test_diagnosis_t` object, whereas
expression `allocated(a)` on the subsequent line evaluates to a `logical` value.
If an assertion containing a Julienne expression fails, Julienne inserts diagnostic
information into the stop code in an ultimate `error stop`.  If an expression
evaluates to a `logical` value of `false.`, the error stop code will contain a
literal copy of the expression (e.g., `allocated(a)`).  In either case, Julienne
also inserts the file and line number into the stop code using via the `__FILE__`
and `__LINE__` macros, respectively.  Most compilers write the resulting stop code
to `error_unit`.

An Origin Story
---------------
Julienne's name derives from the term for vegetables sliced into thin strings:
julienned vegetables.  The [Veggies] and [Garden] unit-testing frameworks
inspired the structure of Julienne's tests and output.  Initially developed in
the [Sourcery] repository as lightweight alternative with greater portability
across compilers, Julienne's chief innovation now lies in the expressive idioms
the framework supports.

Building and Testing
--------------------
### Compiler support
When built with the compiler versions tabulated below, all Julienne tests pass.

Compiler         | Version(s) Tested      | Known Issues
-----------------|------------------------|-------------
LLVM `flang-new` | 19, 20                 | none
NAG `nagfor`     | 7.2 Build 7227         | none
Intel `ifx`      | 2025.2.1               | none
GCC `gfortran`   | 13.4.0, 14.3.0, 15.1.0 | see below

With `gfortran` 13 through 14.2.0,
   - The `test_description_t` constructor's `diagnosis_function` actual argument
     must be a procedure pointer declared with `procedure(diagnosis_function_i)`.
   - The `string_t` type-bound  function `bracket` crashes.

### Build/test commands

#### LLVM (`flang-new`) compiler
##### `flang-new` version 20 or later
```
fpm test --compiler flang-new
```

##### `flang-new` version 19
Add the following command before the `fpm` command recommended above for
LLVM 20 or later:
```bash
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
fpm test --compiler ifx --flag "-fpp -O3" --profile release
```
Older versions of `ifx` might require adding `-coarray` to the quoted argument just after `--flag` above.

Documentation
-------------
See our online [documentation] or build the documentation locally by installing [FORD] and executing `ford ford.md`.

Known Software Using Julienne
-----------------------------
* [Fiats](https://go.lbl.gov/fiats): Functional inference and training for surrogates
* [Matcha](https://go.lbl.gov/matcha): Motility analysis of T-cell histories in activation
* [TRACE](https://www.nrc.gov/docs/ML1200/ML120060218.pdf) two-phase flow solver for nuclear reactors
* nQMCC: Quantum Monte Carlo simulation software for nuclear physics

[#51]: https://github.com/BerkeleyLab/julienne/issues/51
[Assertions]: #assertions
[`diagnosis_function_i`]: https://github.com/BerkeleyLab/julienne/blob/37bcc959efa8f9e27ae50fecfd37a6bf52ef0a43/src/julienne/julienne_test_description_m.f90#L16
[documentation]: https:///berkeleylab.github.io/julienne/
[FORD]: https://github.com/Fortran-FOSS-Programmers/ford
[Garden]: https://gitlab.com/everythingfunctional/garden
[handy-dandy]: https://github.com/rouson/handy-dandy/blob/7caaa4dc3d6e5331914a3025f0cb1db5ac1a886f/src/fresh-llvm-build.sh
[Sourcery]: https://github.com/sourceryinstitute/sourcery
[Veggies]: https://gitlab.com/everythingfunctional/veggies
