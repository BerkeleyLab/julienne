! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_description_test_m
  !! Verify test_description_t object behavior
  use iso_c_binding, only : c_funloc
  use julienne_m, only : &
     string_t &
    ,diagnosis_function_i &
    ,test_result_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,usher &
    ,operator(.also.) &
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
      test_description_t("identical construction from equivalent arguments", usher(check_constructors_match)) &
    ]
    test_results = test_description_test%run(test_descriptions)
  end function

  function check_constructors_match() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(diagnosis_function_i), pointer :: tautology_ptr
    tautology_ptr => tautology

    test_diagnosis = test_diagnosis_t(  &
       test_passed = test_description_t("foo", tautology_ptr) == test_description_t(string_t("foo"), tautology_ptr) &
      ,diagnostics_string= 'test_description_t("foo", tautology_ptr) /= test_description_t(string_t("foo"), tautology_ptr)'&
    )
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_diagnosis = test_diagnosis .also. test_diagnosis_t( &
       test_passed = test_description_t("foo", tautology) == test_description_t(string_t("foo"), tautology) &
      ,diagnostics_string = 'test_description_t("foo", tautology) /= test_description_t(string_t("foo"), tautology)' &
    )
    test_diagnosis = test_diagnosis .also. test_diagnosis_t( &
       test_passed = test_description_t("foo", tautology) == test_description_t("foo", tautology_ptr) &
      ,diagnostics_string = 'test_description_t("foo", tautology) /= test_description_t("foo", tautology_ptr)' &
    )
#endif
    test_diagnosis = test_diagnosis .also. test_diagnosis_t( &
       test_passed = test_description_t("foo", tautology_ptr) == test_description_t("foo", usher(tautology)) &
      ,diagnostics_string= 'test_description_t("foo", tautology_ptr) /= test_description_t("foo", usher(tautology))'&
    )
    test_diagnosis = test_diagnosis .also. test_diagnosis_t( &
       test_passed = test_description_t("foo", tautology_ptr) == test_description_t("foo", c_funloc(tautology)) &
      ,diagnostics_string= 'test_description_t("foo", tautology_ptr) /= test_description_t("foo", c_funloc(tautology))'&
    )
    test_diagnosis = test_diagnosis .also. test_diagnosis_t( &
       test_passed = test_description_t(string_t("foo"), tautology_ptr) == test_description_t(string_t("foo"), usher(tautology)) &
      ,diagnostics_string= 'test_description_t(string_t("foo"), tautology_ptr) /= test_description_t(string_t("foo"), usher(tautology))'&
    )
    test_diagnosis = test_diagnosis .also. test_diagnosis_t( &
       test_passed = test_description_t(string_t("foo"), tautology_ptr) == test_description_t(string_t("foo"), c_funloc(tautology)) &
      ,diagnostics_string= 'test_description_t(string_t("foo"), tautology_ptr) /= test_description_t(string_t("foo"), c_funloc(tautology))'&
    )
  contains
    type(test_diagnosis_t) function tautology()
      tautology = test_diagnosis_t(.true.,"")
    end function
  end function

end module test_description_test_m
