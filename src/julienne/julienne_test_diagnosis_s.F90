! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(julienne_test_diagnosis_m) julienne_test_diagnosis_s
  use assert_m
  implicit none
contains
    module procedure construct_from_string_t
      test_diagnosis%test_passed_ = test_passed
      test_diagnosis%diagnostics_string_ = diagnostics_string
    end procedure

    module procedure construct_from_character
      test_diagnosis%test_passed_ = test_passed
      test_diagnosis%diagnostics_string_ = diagnostics_string
    end procedure

    module procedure test_passed
      passed = self%test_passed_
    end procedure

    module procedure diagnostics_string
      call_assert(allocated(self%diagnostics_string_))
      string_ = string_t(self%diagnostics_string_)
    end procedure
end submodule julienne_test_diagnosis_s
