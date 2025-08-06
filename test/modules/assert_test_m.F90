! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "julienne-assert-macros.h"

module assert_test_m
  !! Test Julienne's assert generic interface

  use julienne_m, only : & 
     call_julienne_assert_ &
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
    specimen = "The call_julienne_assert macro" 
  end function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    associate(descriptions => [ &
       test_description_t("invocation with an expression containing Julienne operators", check_macro_with_julienne_idiom) &
      ,test_description_t("invocation with a logical expression", check_macro_with_logical_assertion) &
    ])
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_test_diagnosis => descriptions%contains_text(test_description_substring))
          associate(matching_descriptions => pack(descriptions, substring_in_subject .or. substring_in_test_diagnosis))
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
       check_macro_with_julienne_idiom_ptr => check_macro_with_julienne_idiom &
      ,check_macro_with_logical_assertion_ptr => check_macro_with_logical_assertion

    descriptions = [ &
       test_description_t("invocation via macro with an expression containing Julienne operators", check_macro_with_expression_ptr)&
      ,test_description_t("invocation with a logical expression", check_macro_with_logical_assertion_ptr) &
    ]

    block
      logical substring_in_subject
      logical, allocatable :: substring_in_test_diagnosis(:)
      type(test_description_t), allocatable :: matching_descriptions(:)

      substring_in_subject = index(subject(), test_description_substring) /= 0
      substring_in_test_diagnosis = descriptions%contains_text(test_description_substring)
      matching_descriptions = pack(descriptions, substring_in_subject .or. substring_in_test_diagnosis)
      test_results = matching_descriptions%run()
    end block
  end function

#endif

  function check_macro_with_julienne_idiom() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call_julienne_assert(1. .approximates. 2. .within. 3.)
    test_diagnosis = test_diagnosis_t(.true., "")
  end function

  function check_macro_with_logical_assertion() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call_julienne_assert(1==1)
  end function

end module assert_test_m
