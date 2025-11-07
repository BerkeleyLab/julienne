! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_result_test_m
  !! Verify test_result_t object behavior
  use julienne_m, only : &
     operator(.expect.) &
    ,string_t &
    ,bless &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
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

    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), bless(check_array_result_construction)) &
    ]
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
