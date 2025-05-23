! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module assert_test_m
  !! Test Julienne's assert generic interface

  use julienne_m, only : & 
     assert &
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
    specimen = "The assert generic interface" 
  end function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    associate(descriptions => [ &
      test_description_t("invocation with an test_diagnosis_t expression", check_test_diagnosis_expression) &
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
    use julienne_m, only : diagnosis_function_i
    !! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: descriptions(:)
    procedure(diagnosis_function_i), pointer :: &
       check_test_diagnosis_expression_ptr => check_test_diagnosis_expression

    descriptions = [ &
      test_description_t("invocation with an test_diagnosis_t expression", check_test_diagnosis_expression_ptr) &
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

  function check_test_diagnosis_expression() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    call assert(1. .approximates. 2. .within. 3.)
    test_diagnosis = test_diagnosis_t(.true., "")
  end function

end module assert_test_m
