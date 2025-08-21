Julienne Classes
----------------
Testing centers around the `test_t` abstract derived type.
Users define a collection of tests by defining `test_t` child types.
This obligates the child type to define `test_t`'s deferred bindings: `subject` and `results`.
The `subject` function producdes a `character` string result describing what is being tested -- often a derived type or module.
The `results` function passes a `test_descripton_t` array to a child instance's inherited type-bound `run` function.
The `run` funtion produces a `test_result_t` array result.

```mermaid
classDiagram

test_t --> test_description_t : "'run' accepts array of"
test_t --> test_result_t : "'run' produces array of"

class test_t{
  <<abstract>>
    + subject() character *
    + results() test_result_t *
    + report(passes : integer, tests : integer, skips : integer)
}

class string_t{
   + string_t(character) : string_t
   + string_t(complex) : string_t
   + string_t(double precision) : string_t
   + string_t(integer) : string_t
   + string_t(logical) : string_t
   + string_t(real) : string_t
   + operator(.cat.) : string_t
   + operator(.csv.) : string_t
   + operator(.sv.) : string_t
   + string() : character
   + array_of_strings(string_t) : string_t
   + bracket(opening : character, closing : closing) : string_t
}
class test_diagnosis_t{
    + test_diagnosis_t(test_passed : logical, diagnostics_string : character) test_diagnosis_t
    + test_diagnosis_t(test_passed : logical, diagnostics_string : string_t) test_diagnosis_t
    + test_passed() logical
    + diagnsotics_string() string_t
}

class test_result_t{
    + test_result_t(description : character, diagnosis : test_diagnosis_t) test_result_t
    + test_result_t(description : string_t, diagnosis : test_diagnosis_t) test_result_t
    + characterize() : character
    + description_contains(string_t) logical
    + description_contains(character) logical
    + passed() logical
    + skipped() logical
}

class test_description_t{
    + test_description_t(description : character, diagnosis_function : procedure(diagnosis_function_i)) test_description_t
    + test_description_t(description : string_t, diagnosis_function : procedure(diagnosis_function_i)) test_description_t
    + run() test_result_t
    + contains_text(character) logical
    + contains_text(string_t) logical
    + operator(==) logical
}
