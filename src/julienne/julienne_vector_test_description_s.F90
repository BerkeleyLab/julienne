! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(julienne_vector_test_description_m) julienne_vector_test_description_s
  use julienne_m, only : call_julienne_assert_
  implicit none

contains

  module procedure contains_characters
    integer i
    call_julienne_assert(allocated(self%descriptions_))
    match_vector = [(index(self%descriptions_(i)%string(), substring) /= 0, i = 1, size(self%descriptions_))]
  end procedure

  module procedure contains_string_t
    match_vector = self%contains_characters(substring%string())
  end procedure

#ifndef __GFORTRAN__

  module procedure construct_from_strings
    vector_test_description%descriptions_ = descriptions
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end procedure

#else

  module function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
    type(string_t), intent(in) :: descriptions(:)
    procedure(vector_diagnosis_function_i), intent(in), pointer, optional :: vector_diagnosis_function
    type(vector_test_description_t) vector_test_description
    vector_test_description%descriptions_ = descriptions
    if (present(vector_diagnosis_function)) vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end function

#endif

  module procedure run
    if (.not. associated(self%vector_diagnosis_function_)) then
      test_results = test_result_t(self%descriptions_)
    else
      associate(diagnoses => self%vector_diagnosis_function_())
#if defined(ASSERTIONS)
        associate(num_descriptions => size(self%descriptions_), num_results => size(diagnoses))
          call_julienne_assert(num_descriptions == num_results)
        end associate
#endif
        test_results = test_result_t(self%descriptions_, diagnoses)
      end associate
    end if
  end procedure

end submodule julienne_vector_test_description_s
