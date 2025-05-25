submodule (julienne_test_diagnosis_m) julienne_assertion_diagnosis_s
  use assert_m, only : assert_library_assert => assert_always
  implicit none

contains

  module procedure construct_from_components
    assertion_diagnosis%test_diagnosis_t = test_diagnosis_t(success, diagnostics_string)
  end procedure

  module procedure construct_with_string
    assertion_diagnosis%test_diagnosis_t = test_diagnosis_t(success, diagnostics_string%string())
  end procedure

  module procedure julienne_assert
    character(len=:), allocatable :: diagnostics_string
    diagnostics_string =  test_diagnosis%diagnostics_string_
    if (present(file)) diagnostics_string = diagnostics_string // " in file " // file
    if (present(line)) diagnostics_string = diagnostics_string // " at line " // string_t(line)
    call assert_library_assert(test_diagnosis%test_passed_, diagnostics_string)
  end procedure

  module procedure assert_with_assertion_diagnosis
    character(len=:), allocatable :: diagnostics_string
    diagnostics_string =  assertion_diagnosis%diagnostics_string_
    if (present(file)) diagnostics_string = diagnostics_string // " in file " // file
    if (present(line)) diagnostics_string = diagnostics_string // " at line " // string_t(line)
    call assert_library_assert(assertion_diagnosis%test_passed_, diagnostics_string)
  end procedure

end submodule
