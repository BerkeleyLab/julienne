! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
#include "language-support.F90"

submodule(julienne_test_diagnosis_m) julienne_test_diagnosis_s
  use assert_m
  implicit none
contains

  module procedure approximates_real
    operands = operands_t(actual, expected) 
  end procedure

  module procedure approximates_double_precision
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
    operands = operands_t(double_precision)(actual, expected) 
#else
    operands = double_precision_operands_t(actual, expected) 
#endif
  end procedure

  module procedure equals_expected_integer

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected " // string_t(expected) // "; actual value is " // string_t(actual) &
      )
    end if

  end procedure

  module procedure less_than_real

    if (actual < expected_ceiling) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than " // string_t(expected_ceiling) &
      )
    end if

  end procedure

  module procedure less_than_double

    if (actual < expected_ceiling) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than " // string_t(expected_ceiling) &
      )
    end if

  end procedure

  module procedure less_than_integer

    if (actual < expected_ceiling) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than " // string_t(expected_ceiling) &
      )
    end if

  end procedure

  module procedure within_real

    if (abs(operands%actual - operands%expected) < tolerance) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // "within a tolerance of " // string_t(tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision

    if (abs(operands%actual - operands%expected) < tolerance) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // "within a tolerance of " // string_t(tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

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
