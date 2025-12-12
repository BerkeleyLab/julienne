! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_diagnosis_test_m
  !! Verify test_diagnosis_t object behavior

  use julienne_m, only : & 
     string_t &
    ,test_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,passing_test &
    ,test_result_t &
    ,usher &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.and.) &
    ,operator(.equalsExpected.) &
    ,operator(.expect.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,operator(.withinFraction.) &
    ,operator(.withinPercentage.) &
    ,operator(.lessThan.) &
    ,operator(.isBefore.) &
    ,operator(.isAfter.) &
    ,operator(.isAtMost.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.)
  use iso_c_binding, only : c_ptr, c_loc, c_bool
  use iso_fortran_env, only : int64
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
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_diagnosis_test_t) test_diagnosis_test

    test_descriptions = [ &
       test_description_t("construction from the real expression 'x .approximates. y .within. tolerance'"                      , usher(check_approximates_real)) &
      ,test_description_t("construction from the real expression 'x .approximates. y .withinFraction. tolerance'"              , usher(check_approximates_real_fraction)) &
      ,test_description_t("construction from the real expression 'x .approximates. y .withinPercentage. tolerance'"            , usher(check_approximates_real_percentage)) &
      ,test_description_t("construction from the real expression 'x .lessThan. y"                                              , usher(check_less_than_real)) &
      ,test_description_t("construction from the real expression 'x .greaterThan. y"                                           , usher(check_greater_than_real)) &
      ,test_description_t("construction from the double precision expression 'x .approximates. y .within. tolerance'"          , usher(check_approximates_double)) &
      ,test_description_t("construction from the double precision expression 'x .approximates. y .withinFraction. tolerance'"  , usher(check_approximates_double_fraction)) &
      ,test_description_t("construction from the double precision expression 'x .approximates. y .withinPercentage. tolerance'", usher(check_approximates_double_percentage)) &
      ,test_description_t("construction from the double precision expression 'x .lessThan. y"                                  , usher(check_less_than_double)) &
      ,test_description_t("construction from the double precision expression 'x .greaterThan. y"                               , usher(check_greater_than_double)) &
      ,test_description_t("construction from string_t/character expressions 'a .isBefore. b'"                                  , usher(check_alphabetical)) &
      ,test_description_t("construction from string_t/character expressions 'a .isAfter. b'"                                   , usher(check_reverse_alphabetical)) &
      ,test_description_t("construction from string_t/character expressions 'a .equalsExpected. b'"                            , usher(check_equals_character_vs_string)) &
      ,test_description_t("construction from the character expression 'a .equalsExpected. b'"                                  , usher(check_equals_character)) &
      ,test_description_t("construction from the type(c_ptr) expression 'p .equalsExpected. q'"                                , usher(check_equals_c_ptr)) &
      ,test_description_t("construction from the logical expression 't .equalsExpected. t'"                                    , usher(check_equals_logical)) &
      ,test_description_t("construction from the string_t expression 'a .equalsExpected. b'"                                   , usher(check_equals_string)) &
      ,test_description_t("construction from the integer expression 'i .equalsExpected. j'"                                    , usher(check_equals_integer)) &
      ,test_description_t("construction from integer(int64) relational operators"                                              , usher(check_int64_comparisons)) &
      ,test_description_t("construction from the integer expression 'i .lessThan. j"                                           , usher(check_less_than_integer)) &
      ,test_description_t("construction from the integer expression '[i,j] .lessThanOrEqualTo. k"                              , usher(check_less_than_or_equal_to_integer)) &
      ,test_description_t("construction from the integer expression 'i .greaterThan. j"                                        , usher(check_greater_than_integer)) &
      ,test_description_t("construction from the integer expression '[i,j] .greaterThanOrEqualTo. k"                           , usher(check_greater_than_or_equal_to_integer)) &
      ,test_description_t("construction from the scalar test_diagnostics_t expression 't .and. u'"                             , usher(check_and_with_scalar_operands)) &
      ,test_description_t("construction from the vector test_diagnostics_t expressions 'i .equalsExpected. [j,k]'"             , usher(check_and_with_vector_operands)) &
      ,test_description_t("construction from string concatenation"                                                             , usher(check_string_concatentation)) &
      ,test_description_t("construction from character concatenation"                                                          , usher(check_character_concatentation)) &
      ,test_description_t("construction from (.expects. logical-expression) // 'user-defined message'"                         , usher(check_expects_logical)) &
      ,test_description_t("construction from (.expects. logical-expression) // 'user-defined message'"                         , usher(check_expects_logical)) &
      ,test_description_t("defining a test_diagnosis_t object by assigning a logical value"                                    , usher(check_assigns_logical)) &
      ,test_description_t("aggregating a test_diagnosis_t object using .also. with a logical value"                                    , usher(check_also_logical)) &
      ,test_description_t("hardwiring a test to pass via the passing_test() function"                                          , usher(check_passing_test_function)) &
      ,test_description_t("construction from another test_diagnosis_t"                                                         , usher(check_copy_construction)) &
    ]
    test_results = test_diagnosis_test%run(test_descriptions)
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

  function check_approximates_real_fraction() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: actual_value = 1.1, expected_value = 1., fraction_ = 2.E-01
    test_diagnosis = actual_value .approximates. expected_value .withinFraction. fraction_
  end function

  function check_approximates_double_fraction() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: actual_value = 1.1D0, expected_value = 1D0, fraction_ = 2D-01
    test_diagnosis = actual_value .approximates. expected_value .withinFraction. fraction_
  end function

  function check_approximates_real_percentage() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: actual_value = 1.01, expected_value = 1., percentage = 2.
    test_diagnosis = actual_value .approximates. expected_value .withinPercentage. percentage
  end function

  function check_approximates_double_percentage() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: actual_value = 1.01D0, expected_value = 1D0, percentage = 2D0
    test_diagnosis = actual_value .approximates. expected_value .withinPercentage. percentage
  end function

  function check_reverse_alphabetical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = (string_t("foo") .isAfter. string_t("bar")) &
               .and. (string_t("foo") .isAfter. "bar") &
               .and. ("foo" .isAfter. "bar") &
               .and. ("foo" .isAfter. string_t("bar"))
  end function

  function check_alphabetical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = (string_t("bar") .isBefore. string_t("foo")) &
               .and. (string_t("bar") .isBefore. "foo") &
               .and. ("bar" .isBefore. "foo") &
               .and. ("bar" .isBefore. string_t("foo"))
  end function

  function check_equals_character() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: expected_value = "foo"
    test_diagnosis = "foo" .equalsExpected. expected_value
  end function

  function check_equals_c_ptr() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    logical(c_bool), target :: t
    type(c_ptr) t_ptr
    t = .true._c_bool
    t_ptr = c_loc(t)
    test_diagnosis = t_ptr .equalsExpected. c_loc(t)
  end function

  function check_equals_logical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    logical, parameter :: t = .true., f = .false.
    test_diagnosis = (t .equalsExpected. t) .also. (f .equalsExpected. f)
  end function

  function check_equals_string() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(string_t) expected_value
    expected_value = string_t("foo")
    test_diagnosis = string_t("foo") .equalsExpected. expected_value
  end function

  function check_equals_character_vs_string() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: expected_character = "foo"
    type(string_t) expected_string
    expected_string = string_t(expected_character)
    test_diagnosis = ("foo" .equalsExpected. expected_string) .and. (string_t("foo") .equalsExpected. expected_character)
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

  function check_int64_comparisons() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer(int64), parameter :: zero = 0_int64, one = 1_int64, two = 2_int64
    test_diagnosis = &
             ( zero .lessThan.    one) &
      .also. ( one  .isAtLeast.   one) &
      .also. (-one  .isAtMost.   zero) &
      .also. ( two  .greaterThan. one)
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
    test_diagnosis = .all. ([0,1] .isAtMost. expected_max)
  end function

  function check_greater_than_or_equal_to_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_min = 1
    test_diagnosis = .all. ([1,2] .isAtLeast. expected_min)
  end function

  function check_and_with_scalar_operands() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_min = 1
    test_diagnosis = (2 .isAtLeast. expected_min) .and. (1 .equalsExpected. 1)
  end function

  function check_and_with_vector_operands() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = .all. ((2 .equalsExpected. [2,2,2]) .and. ([0,1,2] .equalsExpected. [0,1,2]))
  end function

  function check_string_concatentation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

#ifndef __GFORTRAN__
    associate(diagnosis_cat_string => test_diagnosis_t(test_passed=.false., diagnostics_string="blah blah") // string_t(" yada yada"))
      test_diagnosis = diagnosis_cat_string%diagnostics_string() .equalsExpected. "blah blah yada yada"
    end associate
    associate(diagnosis_do_not_cat_string => test_diagnosis_t(test_passed=.true., diagnostics_string="blah blah") // string_t(" yada yada"))
      test_diagnosis = test_diagnosis .also. (diagnosis_do_not_cat_string%diagnostics_string() .equalsExpected. "blah blah")
    end associate
#else
    block
      type(test_diagnosis_t) diagnosis_cat_string, diagnosis_do_not_cat_string

      diagnosis_cat_string = test_diagnosis_t(test_passed=.false., diagnostics_string="blah blah") // string_t(" yada yada")
      test_diagnosis = diagnosis_cat_string%diagnostics_string() .equalsExpected. "blah blah yada yada"

      diagnosis_do_not_cat_string = test_diagnosis_t(test_passed=.true., diagnostics_string="blah blah") // string_t(" yada yada")
      test_diagnosis = test_diagnosis .also. (diagnosis_do_not_cat_string%diagnostics_string() .equalsExpected. "blah blah")
    end block
#endif
  end function

  function check_character_concatentation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
#ifndef __GFORTRAN__
    associate(diagnosis_cat_string => test_diagnosis_t(test_passed=.false., diagnostics_string="blah blah") // " yada yada")
      test_diagnosis = diagnosis_cat_string%diagnostics_string() .equalsExpected. "blah blah yada yada"
    end associate
    associate(diagnosis_do_not_cat_string => test_diagnosis_t(test_passed=.true., diagnostics_string="blah blah") // " yada yada")
      test_diagnosis = test_diagnosis .also. (diagnosis_do_not_cat_string%diagnostics_string() .equalsExpected. "blah blah")
    end associate
#else
    block
      type(test_diagnosis_t) diagnosis_cat_string, diagnosis_do_not_cat_string

      diagnosis_cat_string = test_diagnosis_t(test_passed=.false., diagnostics_string="blah blah") // " yada yada"
      test_diagnosis = diagnosis_cat_string%diagnostics_string() .equalsExpected. "blah blah yada yada"

      diagnosis_do_not_cat_string = test_diagnosis_t(test_passed=.true., diagnostics_string="blah blah") // " yada yada"
      test_diagnosis = test_diagnosis .also. (diagnosis_do_not_cat_string%diagnostics_string() .equalsExpected. "blah blah")
    end block
#endif
  end function

  function check_expects_logical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, allocatable :: A(:)
    test_diagnosis = .expect. (.not. allocated(A)) // "(expected unallocated array A)"
  end function

  function check_assigns_logical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = .true.
  end function

  function check_also_logical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = .true.
    test_diagnosis = test_diagnosis .also. .true.
    test_diagnosis = .true. .also. test_diagnosis
  end function

  function check_passing_test_function() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = passing_test()
  end function

  function check_copy_construction() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(test_diagnosis_t) f,u

    test_diagnosis = .true.

    f = .false.
    u = test_diagnosis_t(f)
    test_diagnosis = test_diagnosis .also. &
      (u%diagnostics_string() .equalsExpected. "")

    u = test_diagnosis_t(f, "foo")
    test_diagnosis = test_diagnosis .also. &
      (u%diagnostics_string() .equalsExpected. "foo")

    u = test_diagnosis_t(f, string_t("bar"))
    test_diagnosis = test_diagnosis .also. &
      (u%diagnostics_string() .equalsExpected. "bar")
  end function

end module test_diagnosis_test_m
