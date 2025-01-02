! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(julienne_test_description_m) julienne_test_description_s
  implicit none
contains
    module procedure construct_from_character_and_test_function
      test_description%description_ = description
      test_description%test_function_ => test_function
    end procedure

    module procedure construct_from_string_t_and_test_function
      test_description%description_ = description
      test_description%test_function_ => test_function
    end procedure

    module procedure construct_from_character_and_diagnosis_function
      test_description%description_ = description
      test_description%diagnosis_function_ => diagnosis_function
    end procedure

    module procedure construct_from_string_t_and_diagnosis_function
      test_description%description_ = description
      test_description%diagnosis_function_ => diagnosis_function
    end procedure

    module procedure run
      associate(testing => associated(self%test_function_), diagnosing => associated(self%diagnosis_function_))
        call_assert(count([testing, diagnosing])==1)     
        if (testing) then
          test_result = test_result_t(self%description_, self%test_function_())
        else
          test_result = test_result_t(self%description_, self%diagnosis_function_())
        end if
      end associate
    end procedure

    module procedure contains_string_t
      match = index(self%description_, substring%string()) /= 0
    end procedure

    module procedure contains_characters
      match = index(self%description_, substring) /= 0
    end procedure

    module procedure equals
      lhs_eq_rhs = (lhs%description_ == rhs%description_) .and. associated(lhs%test_function_, rhs%test_function_)
    end procedure
end submodule julienne_test_description_s
