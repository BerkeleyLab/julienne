Example Test Suite Classes
==========================

Scalar Diagnosis Function
-------------------------
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
