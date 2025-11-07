! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_description_test_m
  !! Verify test_description_t object behavior
  use julienne_m, only : &
     string_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,bless &
    ,test_t
  implicit none

  private
  public :: test_description_test_t

  type, extends(test_t) :: test_description_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_description_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_description_test_t) test_description_test

    test_descriptions = [ &
      test_description_t("identical construction from string_t or character argument", bless(check_constructors_match)) &
    ]
    test_results = test_description_test%run(test_descriptions)
  end function

  function check_constructors_match() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = test_diagnosis_t( &
       test_passed = test_description_t("foo", bless(tautology)) == test_description_t(string_t("foo"), bless(tautology)) &
      ,diagnostics_string = 'test_description_t("foo", tautology) /= test_description_t(string_t("foo"), tautology)' &
    )
  contains
    type(test_diagnosis_t) function tautology()
      tautology = test_diagnosis_t(.true.,"")
    end function
  end function

end module test_description_test_m
