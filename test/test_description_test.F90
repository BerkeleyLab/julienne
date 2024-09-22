! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_description_test_m
  !! Verify test_description_t object behavior
  use julienne_m, only : string_t, test_result_t, test_description_t, test_t, test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
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

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t("identical construction from string_t or character arguments", check_character_constructor) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_character_ptr
    check_character_ptr => check_character_constructor
    test_descriptions = [ &
      test_description_t("identical construction from string_t or character arguments", check_character_ptr) &
    ]
#endif
    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => test_descriptions%contains_text(string_t(test_description_substring)) &
    )
      test_descriptions = pack(test_descriptions, substring_in_subject .or. substring_in_description)
    end associate
    test_results = test_descriptions%run()
  end function

  function check_character_constructor() result(passed)
    logical passed
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    passed = test_description_t("foo", tautology) == test_description_t(string_t("foo"), tautology)
#else
    procedure(test_function_i), pointer :: test_function_ptr
    test_function_ptr => tautology
    passed = test_description_t("foo", test_function_ptr) == test_description_t(string_t("foo"), test_function_ptr)
#endif
  contains
    logical function tautology()
      tautology = .true. 
    end function
  end function

end module test_description_test_m
