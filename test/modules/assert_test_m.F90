! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "julienne-assert-macros.h"
#include "assert_macros.h"

module assert_test_m
  !! Test Julienne's call_julienne_assert generic interface
  use assert_m ! Import call_assert macro
  use julienne_m, only : & 
     call_julienne_assert_ &
    ,julienne_assert &
    ,operator(.equalsExpected.) &
    ,test_diagnosis_t &
    ,test_t &
    ,usher &
    ,test_description_t &
    ,test_result_t &
    ,operator(.approximates.) &
    ,operator(.within.)
  implicit none

  private
  public :: assert_test_t

  type, extends(test_t) :: assert_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The julienne_assert subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(assert_test_t) assert_test

    test_descriptions = [ &
       test_description_t("invocation via the call_julienne_assert macro", usher(check_call_julienne_assert_macro)) &
      ,test_description_t("invocation via direct call", usher(check_julienne_assert_call)) &
      ,test_description_t("invocation removal after undefining the ASSERTIONS macro", usher(check_macro_removal)) &
    ]
    test_results = assert_test%run(test_descriptions)
  end function

  function check_call_julienne_assert_macro() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call_julienne_assert(1. .approximates. 2. .within. 3.)
    test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
  end function

  function check_julienne_assert_call() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call julienne_assert(1. .approximates. 2. .within. 3.)
    test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
  end function

  function check_macro_removal() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
#undef ASSERTIONS
#include "julienne-assert-macros.h"
    call_julienne_assert(5 .equalsExpected. 9)
    test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
  end function

end module assert_test_m
