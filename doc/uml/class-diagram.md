Julienne Unit Testing Classes
-----------------------------
```mermaid
classDiagram

test_t --> test_result_t : produces
test_description_t --> test_diagnosis_t : "uses to construct test_result_t"
test_result_t --> test_diagnosis_t : "accepts as constructor argument"

class test_t{
  <<abstract>>
    + subject() character *
    + results() test_result_t *
    + report(passes : integer, tests : integer)
}

class test_result_t{
    - description_ : character
    - diagnostics_ : character
    - passed : logical
    + test_result_t(description : character, passed : test_diagnosis_t) test_result_t
    + characterize() : character
    + description_contains(character) : logical
    + passed() : logical
}

class test_diagnosis_t{
    - test_passed_ : logical
    - diagnostics_string_ : character
    + test_diagnosis_t(test_passed : logical, diagnostics_string : character) test_diagnosis_t
}

class test_description_t{
    - description_ : character
    - diagnosis_function : procecure(diagnosis_function_i), pointer
    + test_description_t(description : character, diagnosis_function : procedure(diagnosis_function_i))
    + run() test_result_t
    + contains_text(character) logical
}
