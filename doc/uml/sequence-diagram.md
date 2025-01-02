Example Sequence Diagram
------------------------
```mermaid
sequenceDiagram
    main->>specimen_test_t: report(passes, tests)
    test_t ->>command_line_t: flag_value("--contains")
    command_line_t ->> specimen_test_t : test_description_substring
    test_t ->> test_t : subject
    test_t ->> test_t : results
    test_t ->> test_description_t : construct
    test_t ->> test_result_t : construct
    test_t ->> test_result_t : characterize
    test_t ->> test_result_t : passed
