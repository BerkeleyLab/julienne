! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(julienne_test_description_m) julienne_test_description_s
  use assert_m
  implicit none
contains

    module procedure construct_from_character_and_diagnosis_function
      test_description%description_ = description
      test_description%diagnosis_function_ => diagnosis_function
    end procedure

    module procedure construct_from_string_t_and_diagnosis_function
      test_description%description_ = description
      test_description%diagnosis_function_ => diagnosis_function
    end procedure

    module procedure run
      call_assert(associated(self%diagnosis_function_))
      test_result = test_result_t(self%description_, self%diagnosis_function_())
    end procedure

    module procedure contains_string_t
      match = index(self%description_, substring%string()) /= 0
    end procedure

    module procedure contains_characters
      match = index(self%description_, substring) /= 0
    end procedure

    module procedure equals
      lhs_eq_rhs = (lhs%description_ == rhs%description_) .and. associated(lhs%diagnosis_function_, rhs%diagnosis_function_)
    end procedure
end submodule julienne_test_description_s
