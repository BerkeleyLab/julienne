! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "assert_macros.h"

submodule(julienne_test_diagnosis_m) julienne_test_diagnosis_s
  use assert_m
  use julienne_string_m, only : operator(.csv.), operator(.cat.)
  implicit none
contains

  module procedure and
     diagnosis = .all. ([lhs,rhs])
  end procedure 

#ifndef __GFORTRAN__

  module procedure aggregate_diagnosis
    character(len=*), parameter :: new_line_indent = new_line('') // "        "
    integer i
    type(string_t), allocatable :: empty(:)

    select rank(diagnoses)
      rank(0)
        diagnosis = diagnoses
      rank(1)
        diagnosis = test_diagnosis_t( &
           test_passed = all(diagnoses%test_passed_) &
          ,diagnostics_string = .cat. pack( &
             array = [(string_t(new_line_indent // diagnoses(i)%diagnostics_string_), i=1,size(diagnoses))] &
            ,mask  = .not. diagnoses%test_passed_ &
        ) )
      rank default 
        associate(diagnoses_rank => string_t(rank(diagnoses)))
          error stop "aggregate_diagnosis (julienne_test_diagnosis_s): rank " // diagnoses_rank%string() // " unspported"
        end associate
    end select
  end procedure

#else

  module procedure aggregate_scalar_diagnosis
    diagnosis = diagnoses
  end procedure
   
  module procedure aggregate_vector_diagnosis
    character(len=*), parameter :: new_line_indent = new_line('') // "        "
    type(string_t), allocatable :: array(:)
    integer i
    allocate(array(size(diagnoses)))
    do i = 1, size(diagnoses)
      array(i) = string_t(new_line_indent // diagnoses(i)%diagnostics_string_)
    end do
    diagnosis = test_diagnosis_t( &
       test_passed = all(diagnoses%test_passed_) &
      ,diagnostics_string = .cat. pack( &
         array = array &
        ,mask  = .not. diagnoses%test_passed_ &
    ) ) 
  end procedure

  module procedure aggregate_dyad_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_triad_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure
#endif

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

  module procedure less_than_or_equal_to_integer

    if (actual <= expected_max) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than or equal to " // string_t(expected_max) &
      )
    end if

  end procedure

  module procedure less_than_or_equal_to_real

    if (actual <= expected_max) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than or equal to " // string_t(expected_max) &
      )
    end if

  end procedure

  module procedure less_than_or_equal_to_double_precision

    if (actual <= expected_max) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be less than or equal to " // string_t(expected_max) &
      )
    end if

  end procedure

  module procedure greater_than_or_equal_to_integer

    if (actual >= expected_min) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than or equal to " // string_t(expected_min) &
      )
    end if

  end procedure

  module procedure greater_than_or_equal_to_real

    if (actual >= expected_min) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than or equal to " // string_t(expected_min) &
      )
    end if

  end procedure

  module procedure greater_than_or_equal_to_double_precision

    if (actual >= expected_min) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than or equal to " // string_t(expected_min) &
      )
    end if

  end procedure

  module procedure greater_than_real

    if (actual > expected_floor) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than " // string_t(expected_floor) &
      )
    end if

  end procedure

  module procedure greater_than_double

    if (actual > expected_floor) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than " // string_t(expected_floor) &
      )
    end if

  end procedure

  module procedure greater_than_integer

    if (actual > expected_floor) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "The value " // string_t(actual) // " was expected to be greater than " // string_t(expected_floor) &
      )
    end if

  end procedure

  module procedure within_real

    if (abs(operands%actual - operands%expected) <= tolerance) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_real_fraction

    if (abs(operands%actual - operands%expected) <= abs(fractional_tolerance*operands%expected)) then
      ! We use <= to allow for fractional_tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a fractional tolerance of " // string_t(fractional_tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_real_percentage

    if (abs(operands%actual - operands%expected) <= abs(operands%expected*percentage_tolerance/1D02)) then
      ! We use <= to allow for fractional_tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected " // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(percentage_tolerance) // " percent;" &
                           // " actual value is " // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision

    if (abs(operands%actual - operands%expected) <= tolerance) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision_fraction

    if (abs(operands%actual - operands%expected) <= abs(fractional_tolerance*operands%expected)) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "              // string_t(operands%expected) &
                           // " within a fractional tolerance of " // string_t(fractional_tolerance)          &
                           // "; actual value is "     // string_t(operands%actual)   &
      )
    end if

  end procedure

  module procedure within_double_precision_percentage

    if (abs((operands%actual - operands%expected)) <= abs(operands%expected*percentage_tolerance/1D02)) then
      ! We use <= to allow for tolerance=0, which could never be satisfied if we used < instead:
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false. &
        ,diagnostics_string = "expected "               // string_t(operands%expected) &
                           // " within a tolerance of " // string_t(percentage_tolerance) // " percent;" &
                           // " actual value is "       // string_t(operands%actual)   &
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

  module procedure julienne_assert
    character(len=:), allocatable :: diagnostics_string
    diagnostics_string =  test_diagnosis%diagnostics_string_
    if (present(file)) diagnostics_string = diagnostics_string // " in file " // file
    if (present(line)) diagnostics_string = diagnostics_string // " at line " // string_t(line)
    call assert_always(test_diagnosis%test_passed_, diagnostics_string)
  end procedure

end submodule julienne_test_diagnosis_s
