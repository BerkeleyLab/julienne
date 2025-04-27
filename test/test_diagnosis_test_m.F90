! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_diagnosis_test_m
  !! Verify test_diagnosis_t object behavior

  use julienne_m, only : & 
     string_t &
    ,test_t &
    ,test_description_t &
    ,test_description_substring &
    ,test_diagnosis_t &
    ,test_result_t &
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    ,diagnosis_function_i &
#endif
    ,operator(.equalsExpected.) &
    ,operator(.approximates.) &
    ,operator(.within.)
  implicit none

  private
  public :: test_diagnosis_test_t

  type, extends(test_t) :: test_diagnosis_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_diagnosis_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    associate(descriptions => [ &
       test_description_t("contruction from a real expression of the form 'x .approximates. y .within. tolerance'", check_real_approximates) &
      ,test_description_t("contruction from a double precision expression of the form 'x .approximates. y .within. tolerance'", check_double_approximates) &
      ,test_description_t("contruction from an integer expression of the form 'i .equalsExpected. j", check_integer_equals) &
    ] )
#else
     ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
     type(test_diagnosis_t), allocatable :: descriptions(:)
     procedure(diagnosis_function_i), pointer :: check_real_approximates_ptr, check_double_approximates, check_integer_equals
     check_real_approximates_ptr => check_real_approximates
     check_double_approximates_ptr => check_double_approximates

     descriptions = [ &
       test_description_t("contruction from a real expression of the form 'x .approximates. y .within. tolerance'", check_real_approximates_ptr) &
      ,test_description_t("contruction from a double-precision expression of the form 'x .approximates. y .within. tolerance'", check_double_approximates_ptr) &
      ,test_description_t("contruction from an integer expression of the form 'i .equalsExpected. j", check_integer_equals_ptr) &
     ]
#endif

#ifndef __GFORTRAN__
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_test_diagnosis => descriptions%contains_text(test_description_substring))
          associate(matching_descriptions => pack(descriptions, substring_in_subject .or. substring_in_test_diagnosis))
            test_results = matching_descriptions%run()
          end associate
        end associate
      end associate
    end associate
#else
    block
      logical substring_in_subject
      logical, allocatable :: substring_in_test_diagnosis(:)
      type(test_diagnosis_t), allocatable :: matching_descriptions(:)

      substring_in_subject = index(subject(), test_description_substring) /= 0
      substring_in_test_diagnosis = descriptions%contains_text(test_description_substring)
      matching_descriptions = pack(descriptions, substring_in_subject .or. substring_in_test_diagnosis)
      test_results = matching_descriptions%run()
    end block
#endif

  end function

  function check_real_approximates() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: expected_value = 1., tolerance = 1.E-08
    test_diagnosis = 1. .approximates. expected_value .within. tolerance
  end function

  function check_double_approximates() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: expected_value = 1D0, tolerance = 1D-16
    test_diagnosis = 1D0 .approximates. expected_value .within. tolerance
  end function

  function check_integer_equals() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_value = 1
    test_diagnosis = 1 .equalsExpected. expected_value
  end function

end module test_diagnosis_test_m
