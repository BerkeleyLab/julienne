! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "julienne-assert-macros.h"

module assert_test_m
  !! Test Julienne's assert generic interface

  use julienne_m, only : & 
     assertion_diagnosis_t &
    ,call_julienne_assert &
    ,test_diagnosis_t &
    ,test_t &
    ,test_description_t &
    ,test_description_substring &
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

    associate(descriptions => [ &
       test_description_t("invocation via macro with a test_diagnosis_t expression", check_macro_with_expression) &
      ,test_description_t("invocation via macro with an assertion_diagnosis_t argument", check_macro_with_assertion_diagnosis) &
    ])
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_assertion_diagnosis => descriptions%contains_text(test_description_substring))
          associate(matching_descriptions => pack(descriptions, substring_in_subject .or. substring_in_assertion_diagnosis))
            test_results = matching_descriptions%run()
          end associate
        end associate
      end associate
    end associate

  end function

#else

  function results() result(test_results)
    !! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument
    use julienne_m, only : diagnosis_function_i
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: descriptions(:)
    procedure(diagnosis_function_i), pointer :: &
       check_macro_with_expression_ptr => check_macro_with_expression &
      ,check_macro_with_assertion_diagnosis_ptr => check_macro_with_assertion_diagnosis

    descriptions = [ &
       test_description_t("invocation via macro with a test_diagnosis_t expression", check_macro_with_expression_ptr) &
      ,test_description_t("invocation via macro with an assertion_diagnosis_t argument", check_macro_with_assertion_diagnosis_ptr) &
    ]

    block
      logical substring_in_subject
      logical, allocatable :: substring_in_assertion_diagnosis(:)
      type(test_description_t), allocatable :: matching_descriptions(:)

      substring_in_subject = index(subject(), test_description_substring) /= 0
      substring_in_assertion_diagnosis = descriptions%contains_text(test_description_substring)
      matching_descriptions = pack(descriptions, substring_in_subject .or. substring_in_assertion_diagnosis)
      test_results = matching_descriptions%run()
    end block
  end function

#endif

  function check_macro_with_expression() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call_julienne_assert(1. .approximates. 2. .within. 3.)
    test_diagnosis = test_diagnosis_t(.true., "")
  end function

  function check_macro_with_assertion_diagnosis() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call_julienne_assert(assertion_diagnosis_t( success=.true., diagnostics_string="unexpected test failure"))
    test_diagnosis = test_diagnosis_t(.true., "")
  end function

end module assert_test_m
