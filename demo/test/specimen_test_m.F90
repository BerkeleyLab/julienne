! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module specimen_test_m
  !! Example unit test for the specimen_t test subject
  use specimen_m, only : specimen_t
  use julienne_m, only : &
     string_t &
    ,test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_description_substring &
    ,test_diagnosis_t &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.lessThan.)
#if defined(__GFORTRAN__)
  use julienne_m, only : diagnosis_function_i ! work around gfortran's missing Fortran 2008 feature
#endif

  implicit none

  private
  public :: specimen_test_t

  type, extends(test_t) :: specimen_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen_description)
    character(len=:), allocatable :: specimen_description
    specimen_description = "A specimen_t object"
  end function

#if ! defined(__GFORTRAN__)

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

    test_descriptions = [ &
       test_description_t("diagnosing the zero function using Julienne operators", check_zero_using_operators) &
      ,test_description_t("diagnosing the zero function using a diagnosis constructor", check_zero_using_constructor) &
      ,test_description_t("aggregating diagnoses of the zero and one functions using operator(.all.)", check_aggregate_diagnosis) &
      ,test_description_t("(intentional failure to demonstrate diagnostic output)", check_print_diagnosis) &
      ,test_description_t("skipping a test when no diagnosis function is specified") &
    ]
    test_descriptions =  pack( &
       array = test_descriptions &
      ,mask = test_descriptions%contains_text(test_description_substring) .or. index(subject(), test_description_substring)/=0 &
    )
    test_results = test_descriptions%run()
  end function

#else

  function results() result(test_results)
    !! work around missing Fortran 2008 feature in gfortran versions earlier than 15
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    procedure(diagnosis_function_i), pointer :: check_operators_ptr => check_zero_using_operators
    procedure(diagnosis_function_i), pointer :: check_constructor_ptr => check_zero_using_constructor
    procedure(diagnosis_function_i), pointer :: check_aggregate_ptr => check_aggregate_diagnosis
    procedure(diagnosis_function_i), pointer :: check_print_diagnosis_ptr => check_print_diagnosis

    ! Ommitting the optional 2nd argument in the 3rd test_description_t constructor below skips the described
    ! test.  When the test suite runs, it reports the test as skipped and reports a tally of skippped tests.

    test_descriptions = [ &
       test_description_t("diagnosing the zero function using Julienne operators", check_operators_ptr) &
      ,test_description_t("diagnosing the zero function using a diagnosis constructor", check_constructor_ptr) &
      ,test_description_t("aggregating diagnoses of the zero and one functions using operator(.all.)") &
      ,test_description_t("(intentional failure to demonstrate diagnostic output)", check_print_diagnosis_ptr) &
      ,test_description_t("skipping a test when no diagnosis function is specified") &
    ]
    test_descriptions =  pack( &
       array = test_descriptions &
      ,mask = test_descriptions%contains_text(test_description_substring) .or. index(subject(), test_description_substring)/=0 &
    )
    test_results = test_descriptions%run()
  end function
#endif

  function check_zero_using_operators() result(test_diagnosis)
    !! Construct a test diagnosis using Julienne's operator(.approximates.) and operator(.within.)
    type(test_diagnosis_t) test_diagnosis
    type(specimen_t) specimen
    real, parameter :: expected_value = 0., tolerance = 1E-02
    test_diagnosis = specimen%zero() .approximates. expected_value .within. tolerance
  end function

  function check_zero_using_constructor() result(test_diagnosis)
    !! Construct a test diagnosis using Julienne's string_t constructor to define a custom diagnostic string 
    type(test_diagnosis_t) test_diagnosis
    type(specimen_t) specimen
    real, parameter :: expected_value = 0., tolerance = 1E-02
    associate(actual_value =>  specimen%zero())
      test_diagnosis = test_diagnosis_t( &
        test_passed = abs(actual_value - expected_value) < tolerance &
       ,diagnostics_string = "expected " // string_t(expected_value) // "; actual " // string_t(actual_value) &
      )
    end associate
  end function

  function check_aggregate_diagnosis() result(test_diagnosis)
    !! Aggregate two test diagnoses using Julienne's operator(.all.)
    type(test_diagnosis_t) test_diagnosis
    type(specimen_t) specimen
    real, parameter :: expected_ceiling = 10.
    integer, parameter :: expected_integer = 1
    associate(actual_real => specimen%zero(), actual_integer => specimen%one())
      test_diagnosis = .all. [actual_integer .equalsExpected. expected_integer, actual_real .lessThan. expected_ceiling]
    end associate
  end function

  function check_print_diagnosis() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = 2 .lessThan. 1 ! intentional test failure
  end function

end module