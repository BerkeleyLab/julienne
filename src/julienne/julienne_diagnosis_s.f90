! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(julienne_diagnosis_m) julienne_diagnosis_s
  use assert_m
  implicit none
contains
    module procedure construct_from_string_t
      diagnosis%passed_ = passed
      diagnosis%diagnostics_ = diagnostics
    end procedure

    module procedure construct_from_character
      diagnosis%passed_ = passed
      diagnosis%diagnostics_ = diagnostics
    end procedure

    module procedure passed
      test_passed = self%passed_
    end procedure

    module procedure diagnostics
      call_assert(allocated(self%diagnostics_))
      diagnostics_string = string_t(self%diagnostics_)
    end procedure
end submodule julienne_diagnosis_s
