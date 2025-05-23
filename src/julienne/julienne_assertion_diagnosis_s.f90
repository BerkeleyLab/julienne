submodule (julienne_test_diagnosis_m) julienne_assertion_diagnosis_s
  use assert_m, only : assert_library_assert => assert
  implicit none

contains

  module procedure construct_from_components
    assertion_diagnosis%test_diagnosis_t = test_diagnosis_t(success, diagnostics_string)
  end procedure

  module procedure construct_with_string
    assertion_diagnosis%test_diagnosis_t = test_diagnosis_t(success, diagnostics_string%string())
  end procedure

  module procedure julienne_assert
    call assert_library_assert(test_diagnosis%test_passed_, test_diagnosis%diagnostics_string_)
  end procedure

end submodule
