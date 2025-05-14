submodule (assertion_m) assertion_s
  implicit none
contains

  module procedure construct_from_parent
    assertion%test_diagnosis_t = test_diagnosis
  end procedure

  module procedure construct_from_string_t
    assertion%test_diagnosis_t = test_diagnosis_t(assertion_succeeds, diagnostics_string)
  end procedure

  module procedure construct_from_character
    assertion%test_diagnosis_t = test_diagnosis_t(assertion_succeeds, diagnostics_string)
  end procedure

end submodule
