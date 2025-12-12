! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module julienne_test_diagnosis_m
  !! Define abstractions, defined operations, and procedures for writing correctness checks
  use julienne_string_m, only : string_t
  use iso_fortran_env, only : int64
  use iso_c_binding, only : c_ptr
  implicit none

  private
  public :: test_diagnosis_t
  public :: diagnosis_function_i
  public :: passing_test
  public :: operator(//)
  public :: operator(.all.)
  public :: operator(.also.)
  public :: operator(.and.)
  public :: operator(.approximates.)
  public :: operator(.isAfter.)
  public :: operator(.isAtLeast.)
  public :: operator(.isAtMost.)
  public :: operator(.isBefore.)
  public :: operator(.equalsExpected.)
  public :: operator(.expect.)
  public :: operator(.greaterThan.)
  public :: operator(.greaterThanOrEqualTo.)
  public :: operator(.lessThan.)
  public :: operator(.lessThanOrEqualTo.)
  public :: operator(.within.)
  public :: operator(.withinFraction.)
  public :: operator(.withinPercentage.)

  type test_diagnosis_t
    !! Encapsulate test outcome and diagnostic information
    private
    logical :: test_passed_ = .false.
    character(len=:), allocatable :: diagnostics_string_
  contains
    procedure, non_overridable :: test_passed
    procedure, non_overridable ::  diagnostics_string
    generic :: assignment(=) => assign_logical
    procedure, non_overridable, private ::  assign_logical
  end type

  abstract interface
    function diagnosis_function_i() result(test_diagnosis)
      import test_diagnosis_t
      implicit none
      type(test_diagnosis_t) test_diagnosis
    end function
  end interface

  integer, parameter :: default_real = kind(1.), double_precision = kind(1D0)

#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
  type operands_t(k)
    integer, kind :: k = default_real
    real(k) actual, expected 
  end type
#else
  type operands_t
    real actual, expected 
  end type

  type double_precision_operands_t
    double precision actual, expected 
  end type
#endif

  interface operator(//)

    elemental module function append_string_if_test_failed(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(test_diagnosis_t), intent(in) :: lhs
      type(string_t), intent(in) :: rhs
      type(test_diagnosis_t) lhs_cat_rhs
    end function

    elemental module function append_character_if_test_failed(lhs, rhs) result(lhs_cat_rhs)
      implicit none
      class(test_diagnosis_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(test_diagnosis_t) lhs_cat_rhs
    end function

  end interface

  interface operator(.all.)
     
#ifndef __GFORTRAN__

    pure module function aggregate_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(..)
      type(test_diagnosis_t) diagnosis
    end function

#else

    pure module function aggregate_scalar_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_vector_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank2_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank3_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank4_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank5_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank6_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank7_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank8_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank9_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank10_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank11_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank12_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank13_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank14_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

    pure module function aggregate_rank15_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      type(test_diagnosis_t) diagnosis
    end function

#endif
  end interface

  interface operator(.also.)
     
    elemental module function also_DD(lhs, rhs) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: lhs, rhs
      type(test_diagnosis_t) diagnosis
    end function

    elemental module function also_DL(lhs, rhs) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: lhs
      logical, intent(in) :: rhs
      type(test_diagnosis_t) diagnosis
    end function

    elemental module function also_LD(lhs, rhs) result(diagnosis)
      implicit none
      logical, intent(in) :: lhs
      type(test_diagnosis_t), intent(in) :: rhs
      type(test_diagnosis_t) diagnosis
    end function

  end interface

  interface operator(.and.)
     module procedure also_DD
     module procedure also_LD
     module procedure also_DL
  end interface

  interface operator(.approximates.)

    elemental module function approximates_real(actual, expected) result(operands)
      implicit none
      real, intent(in) :: actual, expected
      type(operands_t) operands
    end function

    elemental module function approximates_double_precision(actual, expected) result(operands)
      implicit none
      double precision, intent(in) :: actual, expected
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)) operands
#else
      type(double_precision_operands_t) operands
#endif
    end function

  end interface

  interface operator(.expect.)

    elemental module function expect(expected_true) result(test_diagnosis)
      implicit none
      logical, intent(in) :: expected_true
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.equalsExpected.)

    elemental module function equals_expected_logical(actual, expected) result(test_diagnosis)
      implicit none
      logical, intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_c_ptr(actual, expected) result(test_diagnosis)
      implicit none
      type(c_ptr), intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_integer(actual, expected) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_int64(actual, expected) result(test_diagnosis)
      implicit none
      integer(int64), intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_character(actual, expected) result(test_diagnosis)
      implicit none
      character(len=*), intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_string(actual, expected) result(test_diagnosis)
      implicit none
      type(string_t), intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_character_vs_string(actual, expected) result(test_diagnosis)
      implicit none
      character(len=*), intent(in) :: actual
      type(string_t), intent(in) :: expected
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function equals_expected_string_vs_character(actual, expected) result(test_diagnosis)
      implicit none
      type(string_t), intent(in) :: actual
      character(len=*), intent(in) :: expected
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.lessThan.)

    elemental module function less_than_real(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_double(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_integer(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_int64(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      integer(int64), intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.lessThanOrEqualTo.)

    elemental module function less_than_or_equal_to_integer(actual, expected_max) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_or_equal_to_int64(actual, expected_max) result(test_diagnosis)
      implicit none
      integer(int64), intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_or_equal_to_real(actual, expected_max) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function less_than_or_equal_to_double_precision(actual, expected_max) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.isAtMost.)
    module procedure less_than_or_equal_to_integer
    module procedure less_than_or_equal_to_int64
    module procedure less_than_or_equal_to_real
    module procedure less_than_or_equal_to_double_precision
  end interface

  interface operator(.isAtLeast.)
    module procedure greater_than_or_equal_to_integer
    module procedure greater_than_or_equal_to_int64
    module procedure greater_than_or_equal_to_real
    module procedure greater_than_or_equal_to_double_precision
  end interface

  interface operator(.isBefore.)

    elemental module function alphabetical_character_vs_character(lhs, rhs) result(test_diagnosis)
      implicit none
      character(len=*), intent(in) :: lhs, rhs
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function alphabetical_string_vs_string(lhs, rhs) result(test_diagnosis)
      implicit none
      type(string_t), intent(in) :: lhs, rhs
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function alphabetical_character_vs_string(lhs, rhs) result(test_diagnosis)
      implicit none
      character(len=*), intent(in) :: lhs
      type(string_t), intent(in) :: rhs
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function alphabetical_string_vs_character(lhs, rhs) result(test_diagnosis)
      implicit none
      type(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.isAfter.)

    elemental module function reverse_alphabetical_character_vs_character(lhs, rhs) result(test_diagnosis)
      implicit none
      character(len=*), intent(in) :: lhs, rhs
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function reverse_alphabetical_string_vs_string(lhs, rhs) result(test_diagnosis)
      implicit none
      type(string_t), intent(in) :: lhs, rhs
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function reverse_alphabetical_character_vs_string(lhs, rhs) result(test_diagnosis)
      implicit none
      character(len=*), intent(in) :: lhs
      type(string_t), intent(in) :: rhs
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function reverse_alphabetical_string_vs_character(lhs, rhs) result(test_diagnosis)
      implicit none
      type(string_t), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThanOrEqualTo.)

    elemental module function greater_than_or_equal_to_integer(actual, expected_min) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_min
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_or_equal_to_int64(actual, expected_min) result(test_diagnosis)
      implicit none
      integer(int64), intent(in) :: actual, expected_min
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_or_equal_to_real(actual, expected_min) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_min
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_or_equal_to_double_precision(actual, expected_min) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_min
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThan.)

    elemental module function greater_than_real(actual, expected_floor) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_double(actual, expected_floor) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_integer(actual, expected_floor) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function greater_than_int64(actual, expected_floor) result(test_diagnosis)
      implicit none
      integer(int64), intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.within.)

    elemental module function within_real(operands, tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    elemental module function within_double_precision(operands, tolerance) result(test_diagnosis)
      implicit none
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)), intent(in) :: operands
#else
      type(double_precision_operands_t), intent(in) :: operands
#endif
      double precision, intent(in) :: tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
  end interface

  interface operator(.withinFraction.)

    elemental module function within_real_fraction(operands, fractional_tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: fractional_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    elemental module function within_double_precision_fraction(operands, fractional_tolerance) result(test_diagnosis)
      implicit none
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)), intent(in) :: operands
#else
      type(double_precision_operands_t), intent(in) :: operands
#endif
      double precision, intent(in) :: fractional_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
  end interface

  interface operator(.withinPercentage.)

    elemental module function within_real_percentage(operands, percentage_tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: percentage_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    elemental module function within_double_precision_percentage(operands, percentage_tolerance) result(test_diagnosis)
      implicit none
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)), intent(in) :: operands
#else
      type(double_precision_operands_t), intent(in) :: operands
#endif
      double precision, intent(in) :: percentage_tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
  end interface

  interface test_diagnosis_t

    elemental module function construct_from_string_t(test_passed, diagnostics_string) result(test_diagnosis)
      !! The result is a test_diagnosis_t object with the components defined by the dummy arguments
      implicit none
      logical, intent(in) :: test_passed
      type(string_t), intent(in) :: diagnostics_string
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function construct_from_character(test_passed, diagnostics_string) result(test_diagnosis)
      !! The result is a test_diagnosis_t object with the components defined by the dummy arguments
      implicit none
      logical, intent(in) :: test_passed
      character(len=*), intent(in) :: diagnostics_string
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function copy_construct_from_string_t(diagnosis, diagnostics_string) result(test_diagnosis)
      !! The result is a copy of the provided test_diagnosis_t object, with the appended string
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnosis
      type(string_t), intent(in) :: diagnostics_string
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function copy_construct_from_character(diagnosis, diagnostics_string) result(test_diagnosis)
      !! The result is a copy of the provided test_diagnosis_t object, with the optional appended string
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnosis
      character(len=*), intent(in), optional :: diagnostics_string
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface

    module subroutine assign_logical(lhs, rhs)
      implicit none
      class(test_diagnosis_t), intent(out) :: lhs
      logical, intent(in) :: rhs
    end subroutine

    pure module function passing_test() result(test_diagnosis)
      !! Construct a passing test diagnosis with a zero-length diagnostics string
      implicit none
      type(test_diagnosis_t) test_diagnosis
    end function

    elemental module function test_passed(self) result(passed)
      !! The result is .true. if the test passed on the executing image and false otherwise
      implicit none
      class(test_diagnosis_t), intent(in) :: self
      logical passed
    end function

    pure module function diagnostics_string(self) result(string)
      !! The result is a string describing the condition(s) that caused a test failure or is a zero-length string if no failure
      implicit none
      class(test_diagnosis_t), intent(in) :: self
      character(len=:), allocatable :: string
    end function

  end interface

end module julienne_test_diagnosis_m
