! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_test_description_m) julienne_test_description_s
  implicit none
contains
    module procedure construct_from_character
      test_description%description_ = description
      test_description%test_function_ => test_function
    end procedure

    module procedure construct_from_string_t
      test_description%description_ = description
      test_description%test_function_ => test_function
    end procedure

    module procedure run
      test_result = test_result_t(self%description_, self%test_function_())
    end procedure

    module procedure contains_text
      match = index(self%description_%string(), substring%string()) /= 0
    end procedure

    module procedure equals
      lhs_eq_rhs = (lhs%description_ == rhs%description_) .and. associated(lhs%test_function_, rhs%test_function_)
    end procedure
end submodule julienne_test_description_s
