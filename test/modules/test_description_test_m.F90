! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_description_test_m
  !! Verify test_description_t object behavior
  use julienne_m, only : &
     diagnosis_function_i &
    ,filter &
    ,string_t &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t &
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

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t("identical construction from string_t or character argument", check_constructors_match) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: check_constructors_match_ptr

    check_constructors_match_ptr => check_constructors_match
    test_descriptions = [ &
      test_description_t("identical construction from string_t or character argument", check_constructors_match_ptr) &
    ]
#endif
    associate(matching_descriptions => filter(test_descriptions, subject()))
      test_results = matching_descriptions%run()
    end associate
  end function

  function check_constructors_match() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_diagnosis = test_diagnosis_t( &
       test_passed = test_description_t("foo", tautology) == test_description_t(string_t("foo"), tautology) &
      ,diagnostics_string = 'test_description_t("foo", tautology) /= test_description_t(string_t("foo"), tautology)' &
    )
#else
    procedure(diagnosis_function_i), pointer :: tautology_ptr
    tautology_ptr => tautology

    test_diagnosis = test_diagnosis_t(  &
       test_passed = test_description_t("foo", tautology_ptr) == test_description_t(string_t("foo"), tautology_ptr) &
      ,diagnostics_string= 'test_description_t("foo", tautology_ptr) /= test_description_t(string_t("foo"), tautology__ptr)'&
    )
#endif
  contains
    type(test_diagnosis_t) function tautology()
      tautology = test_diagnosis_t(.true.,"")
    end function
  end function

end module test_description_test_m
