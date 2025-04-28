! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module julienne_test_diagnosis_m
  !! Define an abstraction for describing test outcomes and diagnostic information
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: test_diagnosis_t
  public :: operator(.approximates.)
  public :: operator(.within.)
  public :: operator(.equalsExpected.)
  public :: operator(.lessThan.)
  public :: operator(.lessThanOrEqualTo.)
  public :: operator(.greaterThan.)
  public :: operator(.greaterThanOrEqualTo.)

  type test_diagnosis_t
    !! Encapsulate test outcome and diagnostic information
    private
    logical test_passed_
    character(len=:), allocatable :: diagnostics_string_
  contains
    procedure test_passed
    procedure diagnostics_string
  end type

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

  interface operator(.approximates.)

    pure module function approximates_real(actual, expected) result(operands)
      implicit none
      real, intent(in) :: actual, expected
      type(operands_t) operands
    end function

    pure module function approximates_double_precision(actual, expected) result(operands)
      implicit none
      double precision, intent(in) :: actual, expected
#if HAVE_DERIVED_TYPE_KIND_PARAMETERS
      type(operands_t(double_precision)) operands
#else
      type(double_precision_operands_t) operands
#endif
    end function

  end interface

  interface operator(.equalsExpected.)

    pure module function equals_expected_integer(actual, expected) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.lessThan.)

    pure module function less_than_real(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    pure module function less_than_double(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

    pure module function less_than_integer(actual, expected_ceiling) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_ceiling
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.lessThanOrEqualTo.)

    pure module function less_than_or_equal_to_integer(actual, expected_max) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThanOrEqualTo.)

    pure module function greater_than_or_equal_to_integer(actual, expected_min) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_min
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThan.)

    pure module function greater_than_real(actual, expected_floor) result(test_diagnosis)
      implicit none
      real, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    pure module function greater_than_double(actual, expected_floor) result(test_diagnosis)
      implicit none
      double precision, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

    pure module function greater_than_integer(actual, expected_floor) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_floor
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.within.)

    pure module function within_real(operands, tolerance) result(test_diagnosis)
      implicit none
      type(operands_t), intent(in) :: operands
      real, intent(in) :: tolerance
      type(test_diagnosis_t) test_diagnosis
    end function
   
    pure module function within_double_precision(operands, tolerance) result(test_diagnosis)
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

  end interface

  interface

    elemental module function test_passed(self) result(passed)
      !! The result is .true. if the test passed and false otherwise
      implicit none
      class(test_diagnosis_t), intent(in) :: self
      logical passed
    end function

    elemental module function diagnostics_string(self) result(string_)
      !! The result is a string describing the condition(s) that caused a test failure
      implicit none
      class(test_diagnosis_t), intent(in) :: self
      type(string_t) string_
    end function

  end interface

end module julienne_test_diagnosis_m
