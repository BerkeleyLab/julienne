! Copyright (c) 20242-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(julienne_test_description_m) julienne_test_description_s
  use julienne_m, only : call_julienne_assert_
  implicit none
contains

    module procedure construct_from_characters
      test_description%description_ = description
      if (present(diagnosis_function)) test_description%diagnosis_function_ => diagnosis_function
      call_julienne_assert(allocated(test_description%description_))
    end procedure

    module procedure construct_from_string
      test_description%description_ = description
      if (present(diagnosis_function)) test_description%diagnosis_function_ => diagnosis_function
      call_julienne_assert(allocated(test_description%description_))
    end procedure

    module procedure run
      call_julienne_assert(allocated(self%description_))
      if (associated(self%diagnosis_function_)) then
        test_result = test_result_t(self%description_, self%diagnosis_function_())
      else
        test_result = test_result_t(self%description_)
      end if
    end procedure

    module procedure contains_string_t
      call_julienne_assert(allocated(self%description_))
      match = index(self%description_, substring%string()) /= 0
    end procedure

    module procedure contains_characters
      call_julienne_assert(allocated(self%description_))
      match = index(self%description_, substring) /= 0
    end procedure

    module procedure equals
      call_julienne_assert(allocated(lhs%description_) .and. allocated(rhs%description_))
      lhs_eq_rhs = (lhs%description_ == rhs%description_)
      if (associated(lhs%diagnosis_function_) .and. associated(rhs%diagnosis_function_)) &
        lhs_eq_rhs = lhs_eq_rhs .and. associated(lhs%diagnosis_function_, rhs%diagnosis_function_)
    end procedure
end submodule julienne_test_description_s
