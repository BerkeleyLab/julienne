! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(julienne_vector_test_description_m) julienne_vector_test_description_s
  use assert_m
  implicit none

contains

  module procedure contains_text
    integer i
    call_assert(allocated(self%descriptions_))
    match_vector = [(index(self%descriptions_(i)%string(), substring) /= 0, i = 1, size(self%descriptions_))]
  end procedure

  module procedure construct
    vector_test_description%descriptions_ = descriptions
    vector_test_description%vector_diagnosis_function_ => vector_diagnosis_function
  end procedure

  module procedure run
    associate(diagnoses => self%vector_diagnosis_function_())
#ifdef ASSERTIONS
      associate(num_descriptions => size(self%descriptions_), num_results => size(diagnoses))
        call_assert_diagnose(num_descriptions == num_results, "description/result size match", intrinsic_array_t([num_descriptions, num_results]))
      end associate
#endif
      test_results = test_result_t(self%descriptions_, self%vector_diagnosis_function_())
    end associate
  end procedure

end submodule julienne_vector_test_description_s
