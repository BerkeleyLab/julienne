! Copyright (c) 20242-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "assert_macros.h"
#include "language-support.F90"

submodule(julienne_test_description_m) julienne_test_description_s
  use assert_m
  use julienne_assert_m, only : call_julienne_assert_
  use julienne_command_line_m, only : command_line_t
  implicit none
contains

    module procedure construct_from_characters
      test_description%description_ = description
      if (present(diagnosis_function)) test_description%diagnosis_function_ => diagnosis_function
      call_assert(allocated(test_description%description_))
    end procedure

    module procedure construct_from_string
      test_description%description_ = description
      if (present(diagnosis_function)) test_description%diagnosis_function_ => diagnosis_function
      call_assert(allocated(test_description%description_))
    end procedure

    module procedure run
      call_assert(allocated(self%description_))
      if (associated(self%diagnosis_function_)) then
        test_result = test_result_t(self%description_, self%diagnosis_function_())
      else
        test_result = test_result_t(self%description_)
      end if
    end procedure

    module procedure contains_string_t
      call_assert(allocated(self%description_))
      match = index(self%description_, substring%string()) /= 0
    end procedure

    module procedure contains_characters
      call_assert(allocated(self%description_))
      match = index(self%description_, substring) /= 0
    end procedure

    module procedure equals
      call_assert(allocated(lhs%description_) .and. allocated(rhs%description_))
      lhs_eq_rhs = (lhs%description_ == rhs%description_)
      if (associated(lhs%diagnosis_function_) .and. associated(rhs%diagnosis_function_)) &
        lhs_eq_rhs = lhs_eq_rhs .and. associated(lhs%diagnosis_function_, rhs%diagnosis_function_)
    end procedure

    module procedure filter
      type(command_line_t) command_line

#if  defined(__flang__)
      associate(search_string => command_line%flag_value("--contains"))
        filtered_test_descriptions = &
           pack( array = test_descriptions  &
                ,mask  = index(subject, search_string) /= 0                  & ! subject contains search_string
                         .or. test_descriptions%contains_text(search_string) & ! test_description%description_ contains search_string
        )
      end associate
#else
      block
        character(len=:), allocatable :: search_string
        search_string = command_line%flag_value("--contains")
        filtered_test_descriptions = &
           pack( array = test_descriptions  &
                ,mask  = index(subject, search_string) /= 0                  & ! subject contains search_string
                         .or. test_descriptions%contains_text(search_string) & ! test_description%description_ contains search_string
        )
      end block
#endif
    end procedure
end submodule julienne_test_description_s
