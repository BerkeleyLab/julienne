! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_diagnosis_test_m
  !! Verify test_diagnosis_t object behavior

  use julienne_m, only : & 
     string_t &
    ,test_t &
    ,test_description_t &
    ,test_description_substring &
    ,test_diagnosis_t &
    ,test_result_t &
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    ,diagnosis_function_i &
#endif
    ,operator(.all.) &
    ,operator(.and.) &
    ,operator(.equalsExpected.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,operator(.lessThan.) &
    ,operator(.lessThanOrEqualTo.) &
    ,operator(.greaterThan.) &
    ,operator(.greaterThanOrEqualTo.)
  implicit none

  private
  public :: test_diagnosis_test_t

  type, extends(test_t) :: test_diagnosis_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_diagnosis_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    associate(descriptions => [ &
       test_description_t("contruction from a real expression of the form 'x .approximates. y .within. tolerance'", check_approximates_real) &
      ,test_description_t("contruction from a double precision expression of the form 'x .approximates. y .within. tolerance'", check_approximates_double) &
      ,test_description_t("contruction from an integer expression of the form 'i .equalsExpected. j", check_equals_integer) &
      ,test_description_t("contruction from a real expression of the form 'x .lessThan. y", check_less_than_real) &
      ,test_description_t("contruction from a double precision expression of the form 'x .lessThan. y", check_less_than_double) &
      ,test_description_t("contruction from a integer expression of the form 'i .lessThan. j", check_less_than_integer) &
      ,test_description_t("contruction from a real expression of the form 'x .greaterThan. y", check_greater_than_real) &
      ,test_description_t("contruction from a double precision expression of the form 'x .greaterThan. y", check_greater_than_double) &
      ,test_description_t("contruction from a integer expression of the form 'i .greaterThan. j", check_greater_than_integer) &
      ,test_description_t("contruction from a integer expression of the form '[i,j] .lessThanOrEqualTo. k", check_less_than_or_equal_to_integer) &
      ,test_description_t("contruction from a integer expression of the form '[i,j] .greaterThanOrEqualTo. k", check_greater_than_or_equal_to_integer) &
      ,test_description_t("contruction from a test_diagnostics_t expression of the form 't .and. u'", check_and_operator) &
    ] )
#else
     ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
     type(test_description_t), allocatable :: descriptions(:)
     procedure(diagnosis_function_i), pointer :: &
        check_approximates_real_ptr &
       ,check_approximates_double_ptr          , check_equals_integer_ptr &
       ,check_less_than_real_ptr               , check_greater_than_real_ptr &
       ,check_less_than_double_ptr             , check_greater_than_double_ptr &
       ,check_less_than_integer_ptr            , check_greater_than_integer_ptr &
       ,check_less_than_or_equal_to_integer_ptr, check_greater_than_or_equal_to_integer_ptr &
       ,check_and_operator_ptr

     check_approximates_real_ptr                => check_approximates_real
     check_approximates_double_ptr             => check_approximates_double
     check_equals_integer_ptr                   => check_equals_integer
     check_less_than_real_ptr                   => check_less_than_real
     check_less_than_double_ptr                 => check_less_than_double
     check_less_than_integer_ptr                => check_less_than_integer
     check_less_than_or_equal_to_integer_ptr    => check_less_than_or_equal_to_integer
     check_greater_than_real_ptr                => check_greater_than_real
     check_greater_than_double_ptr              => check_greater_than_double
     check_greater_than_integer_ptr             => check_greater_than_integer
     check_greater_than_or_equal_to_integer_ptr => check_greater_than_or_equal_to_integer
     check_and_operator_ptr                     => check_and_operator

     descriptions = [ &
       test_description_t("contruction from a real expression of the form `x .approximates. y .within. tolerance`"            , check_approximates_real_ptr) &
      ,test_description_t("contruction from a double-precision expression of the form `x .approximates. y .within. tolerance`", check_approximates_double_ptr) &
      ,test_description_t("contruction from an integer expression of the form `i .equalsExpected. j`"                         , check_equals_integer_ptr) &
      ,test_description_t("contruction from a real expression of the form 'x .lessThan. y"                                    , check_less_than_real_ptr) &
      ,test_description_t("contruction from a double precision expression of the form 'x .lessThan. y"                        , check_less_than_double_ptr) &
      ,test_description_t("contruction from a integer expression of the form 'i .lessThan. j"                                 , check_less_than_integer_ptr) &
      ,test_description_t("contruction from a integer expression of the form 'i .lessThanOrEqualTo. j"   ) & ! skip check_less_than_or_equal_to_integer_ptr
      ,test_description_t("contruction from a real expression of the form 'x .greaterThan. y"                                 , check_greater_than_real_ptr) &
      ,test_description_t("contruction from a double precision expression of the form 'x .greaterThan. y"                     , check_greater_than_double_ptr) &
      ,test_description_t("contruction from a integer expression of the form 'i .greaterThan. j"                              , check_greater_than_integer_ptr) &
      ,test_description_t("contruction from a integer expression of the form 'i .greaterThanOrEqualTo. j") & ! skip check_greater_than_or_equal_to_integer_ptr
      ,test_description_t("contruction from a test_diagnostics_t expression of the form 't .and. u'"     ) & ! skip check_and_operator_ptr
     ]
#endif

#ifndef __GFORTRAN__
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_test_diagnosis => descriptions%contains_text(test_description_substring))
          associate(matching_descriptions => pack(descriptions, substring_in_subject .or. substring_in_test_diagnosis))
            test_results = matching_descriptions%run()
          end associate
        end associate
      end associate
    end associate
#else
    block
      logical substring_in_subject
      logical, allocatable :: substring_in_test_diagnosis(:)
      type(test_description_t), allocatable :: matching_descriptions(:)

      substring_in_subject = index(subject(), test_description_substring) /= 0
      substring_in_test_diagnosis = descriptions%contains_text(test_description_substring)
      matching_descriptions = pack(descriptions, substring_in_subject .or. substring_in_test_diagnosis)
      test_results = matching_descriptions%run()
    end block
#endif

  end function

  function check_approximates_real() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: expected_value = 1., tolerance = 1.E-08
    test_diagnosis = 1. .approximates. expected_value .within. tolerance
  end function

  function check_approximates_double() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: expected_value = 1D0, tolerance = 1D-16
    test_diagnosis = 1D0 .approximates. expected_value .within. tolerance
  end function

  function check_equals_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_value = 1
    test_diagnosis = 1 .equalsExpected. expected_value
  end function

  function check_less_than_real() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: expected_ceiling = 1.
    test_diagnosis = 0. .lessThan. expected_ceiling
  end function

  function check_less_than_double() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: expected_ceiling = 1D0
    test_diagnosis = 0D0 .lessThan. expected_ceiling
  end function

  function check_less_than_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_ceiling = 1
    test_diagnosis = 0 .lessThan. expected_ceiling
  end function

  function check_greater_than_real() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: expected_floor = 1.
    test_diagnosis = 2. .greaterThan. expected_floor
  end function

  function check_greater_than_double() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: expected_floor = 1D0
    test_diagnosis = 2D0 .greaterThan. expected_floor
  end function

  function check_greater_than_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_floor = 1
    test_diagnosis = (2 .greaterThan. expected_floor)
  end function

  function check_less_than_or_equal_to_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_max = 1
    test_diagnosis = .all. ([0,1] .lessThanOrEqualTo. expected_max)
  end function

  function check_greater_than_or_equal_to_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_min = 1
    test_diagnosis = .all. ([1,2] .greaterThanOrEqualTo. expected_min)
  end function

  function check_and_operator() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_min = 1
    test_diagnosis = (2 .greaterThanOrEqualTo. expected_min) .and. (1 .equalsExpected. 1)
  end function

end module test_diagnosis_test_m
