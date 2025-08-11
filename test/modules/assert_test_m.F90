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
    ,filter &
    ,julienne_assert &
    ,operator(.equalsExpected.) &
    ,test_diagnosis_t &
    ,test_t &
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

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

    test_descriptions = [ &
       test_description_t("invocation via the call_julienne_assert macro", check_call_julienne_assert_macro) &
      ,test_description_t("invocation via direct call", check_julienne_assert_call) &
      ,test_description_t("invocation removal after undefining the ASSERTIONS macro", check_macro_removal) &
    ]
    associate(matching_descriptions => filter(test_descriptions, subject()))
      test_results = matching_descriptions%run()
    end associate
  end function

#else

  function results() result(test_results)
    !! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument
    use julienne_m, only : diagnosis_function_i
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    procedure(diagnosis_function_i), pointer :: &
       check_call_julienne_assert_macro_ptr => check_call_julienne_assert_macro &
      ,check_julienne_assert_call_ptr => check_julienne_assert_call &
      ,check_macro_removal_ptr => check_macro_removal

    test_descriptions = [ &
       test_description_t("invoking the call_julienne_assert macro", check_call_julienne_assert_macro_ptr) &
      ,test_description_t("directly calling julienne_assert", check_julienne_assert_call_ptr) &
      ,test_description_t("removal when the ASSERTIONS macro is defined as 0", check_macro_removal_ptr) &
    ]
    associate(matching_descriptions => filter(test_descriptions, subject()))
      test_results = matching_descriptions%run()
    end associate
  end function

#endif

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
