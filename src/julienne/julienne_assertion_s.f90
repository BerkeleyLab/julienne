submodule (julienne_test_diagnosis_m) julienne_assertion_diagnosis_s
  use assert_m, only : assert_library_assert => assert
  implicit none

contains

  module procedure construct_from_parent
    assertion_diagnosis%test_diagnosis_t = test_diagnosis
  end procedure

  module procedure julienne_assert
    call assert_library_assert(assertion_diagnosis%test_passed_, assertion_diagnosis%diagnostics_string_)
  end procedure

end submodule
