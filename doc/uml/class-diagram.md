Julienne Unit Testing Classes
-----------------------------
```mermaid
classDiagram

test_t --> test_result_t : produces
test_description_t --> test_diagnosis_t : "'run' uses to construct test_result_t"
vector_test_description_t --> test_diagnosis_t : "'run' uses to construct test_result_t array"
test_result_t --> test_diagnosis_t : "accepts as constructor argument"

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

class vector_test_description_t{
    + vector_test_description_t(description : string_t, vector_diagnosis_function : procedure(vector_diagnosis_function_i)) test_description_t
    + run() test_result_t
    + contains_text(character) logical
    + contains_text(string_t) logical
}

class command_line_t{}
class file_t{}
