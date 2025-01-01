! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module specimen_test_m
  !! Example unit test for the specimen_t test subject
  use specimen_m, only : specimen_t
  use julienne_m, only : &
    string_t, &
    test_t, &
    test_result_t, &
    test_description_t, &
    test_description_substring, &
    test_diagnosis_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_diagnosis_function_i ! work around gfortran's missing Fortran 2008 feature
#endif
    
  implicit none

  private
  public :: specimen_test_t

  type, extends(test_t) :: specimen_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen_description)
    character(len=:), allocatable :: specimen_description
    specimen_description = "A specimen_t object" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
      test_description_t("the type-bound function zero() producing a result of 0", check_zero) &
    ]   
#else 
    ! work around gfortran's missing Fortran 2008 feature
    procedure(test_function_i), pointer :: check_zero
    check_zero_ptr => check_zero

    test_descriptions = [ & 
      test_description_t("the type-bound function zero() producing a result of 0", check_zero_ptr) &
    ]   
#endif
    associate(test_subset => pack(test_descriptions, test_descriptions%contains_text(test_description_substring) .or. index(subject(), test_description_substring)/=0))
      test_results = test_subset%run()
    end associate

  end function

  function check_zero() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(specimen_t) specimen
    integer, parameter :: expected_value = 0

    associate(actual_value => specimen%zero())
      test_diagnosis = test_diagnosis_t( &
         test_passed = actual_value == expected_value &
        ,diagnostics_string = "expected value " // string_t(expected_value) //", actual value " // string_t(actual_value) &
      )
    end associate
  end function

end module