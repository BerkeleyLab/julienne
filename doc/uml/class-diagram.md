Julienne Unit Testing Classes
-----------------------------

---
  config:
    class:
      hideEmptyMembersBox: true
---
```mermaid
classDiagram

class string_t
class test_result_t
class test_description_t
class test_diagnosis_t

class test_t{
  <<abstract>>
    + subject() character(len=:)
    + results() test_result_t
    + report(passes : integer, tests : integer)
}
class vector_test_description_t{
  <<abstract>>
    - description_vector_ : string_t
    - vector_function_strategy_ : vector_function_strategy_t
    + vector_function() string_t
}

vector_test_description_t o-- string_t
