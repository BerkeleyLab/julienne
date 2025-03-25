Example Test Suite Classes
==========================

Supported Compilers 
-------------------

Compiler         | Version(s) Tested        | Known Issues
-----------------|--------------------------|-------------
LLVM `flang-new` | 19, 20                   | none
Intel `ifx`      | 2025.4                   | none
NAG `nagfor`     | 7.2 Build 7227           | none
GCC `gfortran`   | 14.1.0, 14.2.0_1, 15.0.1 | 1-3 below

### GFortran Issues
Compiler bugs related to the following issues have been reported to the GCC project:

1. The `test_description_t` constructor's `diagnosis_function` actual argument must be a procedure pointer.
2. Each element of a `vector_test_description_t` array must be defined in a separate program statement.
3. If executed, the `string_t` type's `bracket` procedure causes a program crash.

Some related `gfortran` fixes have already been developed and pushed to the GCC branches for release with GCC 14.3 and 15.1.0.

Getting Started
---------------
Likely the fastest way to get started with Julienne is to copy the source code in this directory and modify it for your purposes:

1. If you build your project with the Fortran Package Manager ([`fpm`](https://github.com/fotran-lang/fpm)), then you might move the `main.F9` and `specimen_test_.F90` files from this subdiretory to a `test/` subdirectory in the root of your project's source tree.
2. Rename the `specimen_test_m.F90`, `specimen_test_m`, and `specimen_test_t` file, module, and type, respectively, replacing `specimen` with the name of an entity that you intend to test -- most likely a module containing procedures or derived type with type-bound procedures.
3. Similarly replace occurrences of `specimen` in the resulting`test/main.F90` file.
4. Modify the `test_descriptions_t` array constructor in your new `*_test_m.F90` file, adding elements for each test to be performed:
```fortran
  test_descriptions = [ & 
    test_description_t("the type-bound function zero() producing a result of 0", check_zero) &
  ]  
```
5. Replace "the type-bound..." string with a description of a test.  The test output will read most naturally if your description contains a gerund: a noun formed from verb ending in "ing" such as `producing` above.
6. Replace the `check_zero` function name with the name of a function that will perform your test. 
7. Edit the correspondingly-renamed function to perform the test.  The function must take no arguments and define a `test_diagnosis_t` result:
```fortran
  test_diagnosis = test_diagnosis_t( &
     test_passed = actual_value == expected_value &
    ,diagnostics_string = "expected value " // string_t(expected_value) //", actual value " // string_t(actual_value) &
  )   
```
The above `test_diagnosis_t` constructor function invocation demonstrates the recommended pattern for writing tests with Julienne:

* Define the `test_passed` keyword argument by writing an expression that will evaluate to `.true.` if and only if the test succeeds.
* Define the `diagnostics_string` keyword argument from own strings and `string_t` constructor invocations, all separated by the concatenation operaor `//`.

`String_t` is a generic interface to various specific functions, each of takes an argument of a different data type, kind, and rank (TKR) and defines a `string_t` result containing a charater representation of the function argument.
Please see Julienne's online [documentation](https:///berkeleylab.github.io/julienne/) for the currently supported TKR.
Please submit an issue to request support for additional TKR or submit a pull request to contribute such support.

#### Forming diagnostic strings from array data

An especially useful pattern for forming diagnostic string involves invoking Julienne's `operator(.csv.)` to produce a string of comma-separated values (CSV) from a one-dimensional (1D) array.
For example, consider the following test description:
```fortran
  test_description_t(" returning the counting numbers up to 3", check_counting_numbers)
```
and the following corresponding test:
```fortran
  function check_counting_numbers()
     integer, parameter :: expected_array(*) = [1, 2, 3]

     associate(actual_array => counting_numbers(max=3))
       test_diagnosis = test_diagnosis_t( &
          test_passed = all(expected_array == actual_array) &
         ,diagnostics_string = "expected " // .csv. string_t(expected_array) // "; actual  // .csv. string_t(actual_array) &
       )
     end associate
  end function
```
If the `counting_numbers` result contains all zeros, the test report would include the following text:
```
FAILS  on returning the counting numbers up to 3 
      diagnostics: expected 1,2,3; actual 0,0,0
```
To support a common array notation, Julienne also supports bracketing strings.

**Exercise 1:** Make `check_counting_numbers` more robust by testing the equivalence of `expected_array` and `actual_array` only if the array sizes match and by treating a size-mismatch as a test failure.

**Exercise 2:** Revise `check_counting_numbers` by defining CSV strings `expected_string` and `actual_string` _before_ invoking `test_diagnostics_t`.
Bracket the CSV strings in the `diagnostics_string` keyword argument by invoking `bracket` type-bound procedure, e.g., `expected_string%bracket()`.

Scalar Diagnosis Function
-------------------------
The Unified Modeling Language ([UML](https://wikipedia.org/Unified_modeling_langauge)) class diagram below depicts the class relationships involved in making the above example work:

```mermaid
 %%{init: { 'theme':'default',  "class" : {"hideEmptyMembersBox": true} } }%%
classDiagram

class test_t{
    <<abstract>>
    subject() character(len=:) *
    results() test_result_t[0..*] *
    report(passes : integer, tests : integer, skips : integer)
}
test_t --> specimen_test_t : report() invokes subject() and results()

class specimen_test_t{
    subject() character(len=:)
    results() test_result_t[0..*]  
}
specimen_test_t --|> test_t : extends and implements
specimen_test_t --> test_description_t : results() constructs local array of
specimen_test_t --> test_description_t : results() invokes run() on

class test_description_t{
    test_description_t(description : string_t, diagnosis_function : diagnosis_function_i)
    run() test_result_t
}
test_description_t --> test_diagnosis_t : run() invokes diagnosis_function to construct
test_description_t --> test_result_t : run() constructs with test_diagnosis_t object

class test_result_t{
    test_result_t(test_passed : logical, diagnosis : test_diagnosis_t)
}

class test_diagnosis_t{
    test_diagnosis_t(test_passed : logical, diagnostics_string : string_t)
}
```

Vector Diagnosis Function
-------------------------
The UML class diagram below depicts the class relationships involved when test function performs multiple checks and defines a result containing an array of corresponding `test_diagnosis_t` objects:
```mermaid
 %%{init: { 'theme':'default',  "class" : {"hideEmptyMembersBox": true} } }%%
classDiagram

class vector_test_description_t{
    vector_test_description_t(description : string_t[1..*], vector_diagnosis_function : vector_diagnosis_function_i)
    run() test_result_t[1..*]
}
vector_test_description_t --> test_diagnosis_t : run() invokes vector_diagnosis_function to construct array of
vector_test_description_t --> test_result_t : run() uses test_diagnostics_t array to construct array of

class test_result_t{
    test_result_t(test_passed : logical, diagnosis : test_diagnosis_t)
}

class test_diagnosis_t{
    test_diagnosis_t(test_passed : logical, diagnostics_string : string_t)
}
```
