! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_result_test_m
  !! Verify test_result_t object behavior
  use julienne_m, only : &
     operator(.expect.) &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: test_result_test_t

  type, extends(test_t) :: test_result_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_result_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_result_test_t) test_result_test

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), check_array_result_construction) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: &
    check_array_ptr => check_array_result_construction
    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), check_array_ptr) &
    ]
#endif
    test_results = test_result_test%run(test_descriptions)
  end function

  function check_array_result_construction() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

#ifndef __GFORTRAN__
    associate(two_test_results => test_result_t(["foo","bar"], [test_diagnosis_t(.true.,""), test_diagnosis_t(.true.,"")]))
      associate(num_results => size(two_test_results))
        test_diagnosis = test_diagnosis_t( &
           test_passed = num_results == 2 &
          ,diagnostics_string = "expected 2, actual " // string_t(num_results) &
        )
      end associate
    end associate

#else
    block
      integer num_results
      type(test_result_t), allocatable :: two_test_results(:)

      two_test_results = test_result_t(["foo","bar"], [test_diagnosis_t(.true.,""), test_diagnosis_t(.true.,"")])
      num_results = size(two_test_results)

      test_diagnosis = test_diagnosis_t( &
         test_passed = num_results == 2 &
        ,diagnostics_string = "expected 2, actual " // string_t(num_results) &
      )
    end block
#endif

  end function

end module test_result_test_m
