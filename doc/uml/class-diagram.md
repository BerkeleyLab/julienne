Julienne Unit Testing Classes
-----------------------------
```mermaid
classDiagram

class string_t
class test_t
class test_result_t
class test_description_t
class test_diagnosis_t
class vector_test_description_t

class test_t{
  <<abstract>>
    + subject() character
    + results() test_result_t
    + report()
}
class vector_test_description_t{
  <<abstract>>
    - description_vector_ : string_t
    - vector_function_strategy_ : vector_function_strategy_t
    + vector_function() string_t
}

vector_test_description_t o-- string_t
