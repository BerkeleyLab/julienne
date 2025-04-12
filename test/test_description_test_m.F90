! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_description_test_m
  !! Verify test_description_t object behavior
  use julienne_m, only : &
     diagnosis_function_i &
    ,string_t &
    ,test_result_t &
    ,test_description_t &
    ,test_description_substring &
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

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    associate(descriptions => &
      [test_description_t("identical construction from string_t or character argument", check_constructors_match)] &
    )
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: check_constructors_match_ptr
    type(test_description_t), allocatable :: descriptions(:)

    check_constructors_match_ptr => check_constructors_match
    descriptions = [ &
      test_description_t("identical construction from string_t or character argument", check_constructors_match_ptr) &
    ]
#endif

#ifndef __GFORTRAN__
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_test_description => descriptions%contains_text(test_description_substring))
          associate(matching_descriptions => pack(descriptions, substring_in_subject .or. substring_in_test_description))
            test_results = matching_descriptions%run()
          end associate
        end associate
      end associate
    end associate

#else
    block
      logical substring_in_subject
      logical, allocatable :: substring_in_test_description(:)
      type(test_description_t), allocatable :: matching_descriptions(:)

      substring_in_subject = index(subject(), test_description_substring) /= 0
      substring_in_test_description = descriptions%contains_text(test_description_substring)
      matching_descriptions = pack(descriptions, substring_in_subject .or. substring_in_test_description)
      test_results = matching_descriptions%run()
    end block
#endif

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
