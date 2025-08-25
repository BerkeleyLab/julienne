! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "assert_macros.h"

submodule(julienne_test_diagnosis_m) julienne_test_diagnosis_s
  use assert_m
  use julienne_string_m, only : operator(.cat.)
  use iso_c_binding, only : c_associated, c_intptr_t
  implicit none
contains

  module procedure append_string_if_test_failed
    if (lhs%test_passed_) then
      lhs_cat_rhs = lhs
    else
      lhs_cat_rhs = test_diagnosis_t(lhs%test_passed_, lhs%diagnostics_string_ // rhs)
    end if
  end procedure

  module procedure append_character_if_test_failed
    if (lhs%test_passed_) then
      lhs_cat_rhs = lhs
    else
      lhs_cat_rhs = test_diagnosis_t(lhs%test_passed_, lhs%diagnostics_string_ // rhs)
    end if
  end procedure

  module procedure also
     diagnosis = .all. ([lhs,rhs])
  end procedure 

#ifndef __GFORTRAN__

  module procedure aggregate_diagnosis
    character(len=*), parameter :: new_line_indent = new_line('') // "        "

    select rank(diagnoses)
      rank(0)
        diagnosis = diagnoses
      rank(1)
        diagnosis = aggregate_vector_diagnosis(diagnoses)
      rank(2)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(3)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(4)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(5)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(6)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(7)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(8)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(9)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(10)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(11)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(12)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(13)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(14)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank(15)
        diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
      rank default 
        associate(diagnoses_rank => string_t(rank(diagnoses)))
          error stop "aggregate_diagnosis (julienne_test_diagnosis_s): rank " // diagnoses_rank%string() // " unspported"
        end associate
    end select

  contains

    pure function aggregate_vector_diagnosis(diagnoses) result(diagnosis)
      type(test_diagnosis_t), intent(in) :: diagnoses(:)
      type(test_diagnosis_t) diagnosis
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
    end function

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

  module procedure aggregate_rank2_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank3_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank4_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank5_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank6_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank7_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank8_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank9_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank10_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank11_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank12_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank13_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank14_diagnosis
    diagnosis = aggregate_vector_diagnosis(reshape(diagnoses, shape=[size(diagnoses)]))
  end procedure

  module procedure aggregate_rank15_diagnosis
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

  module procedure alphabetical_character_vs_character

    if (lhs < rhs) then
      test_diagnosis = test_diagnosis_t(.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(.false., diagnostics_string = rhs //" is before " // lhs // " alphabetically.")
    end if

  end procedure

  module procedure alphabetical_string_vs_string

    if (lhs%string() < rhs%string()) then
      test_diagnosis = test_diagnosis_t(.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(.false., diagnostics_string = lhs //" is before " // rhs // " alphabetically.")
    end if

  end procedure

  module procedure alphabetical_character_vs_string

    if (lhs < rhs%string()) then
      test_diagnosis = test_diagnosis_t(.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(.false., diagnostics_string = lhs //" is before " // rhs // " alphabetically.")
    end if

  end procedure

  module procedure alphabetical_string_vs_character

    if (lhs%string() < rhs) then
      test_diagnosis = test_diagnosis_t(.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(.false., diagnostics_string = lhs //" is before " // rhs // " alphabetically.")
    end if

  end procedure

  module procedure reverse_alphabetical_character_vs_character
    test_diagnosis = rhs .isBefore. lhs
  end procedure

  module procedure reverse_alphabetical_string_vs_string
    test_diagnosis = rhs .isBefore. lhs
  end procedure

  module procedure reverse_alphabetical_character_vs_string
    test_diagnosis = rhs .isBefore. lhs
  end procedure

  module procedure reverse_alphabetical_string_vs_character
    test_diagnosis = rhs .isBefore. lhs
  end procedure

  module procedure expect
    if (expected_true) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed=.false., diagnostics_string="expected to be true")
    end if
  end procedure

  module procedure equals_expected_c_ptr

    if (c_associated(actual, expected)) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      block
        integer(c_intptr_t) actual_c_loc, expected_c_loc
        integer(c_intptr_t), parameter :: mold = 0_c_intptr_t

        associate(actual_c_loc => transfer(actual, mold), expected_c_loc => transfer(expected, mold))
          test_diagnosis = test_diagnosis_t( &
             test_passed = .false. &
            ,diagnostics_string = "expected " // string_t(expected_c_loc) // "; actual value is " // string_t(actual_c_loc) &
          )
        end associate
     end block
    end if

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

  module procedure equals_expected_integer_c_size_t

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected " // string_t(expected) // "; actual value is " // string_t(actual) &
      )
    end if

  end procedure

  module procedure equals_expected_character

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected '" // expected // "'; actual value is '" // actual //"'" &
      )
    end if

  end procedure

  module procedure equals_expected_character_vs_string

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected '" // expected // "'; actual value is '" // actual //"'" &
      )
    end if

  end procedure

  module procedure equals_expected_string

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected '" // expected // "'; actual value is '" // actual //"'" &
      )
    end if

  end procedure

  module procedure equals_expected_string_vs_character

    if (actual == expected) then
      test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
    else
      test_diagnosis = test_diagnosis_t(test_passed = .false. &
        ,diagnostics_string = "expected '" // expected // "'; actual value is '" // actual //"'" &
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

end submodule julienne_test_diagnosis_s
