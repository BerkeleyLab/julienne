! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module julienne_test_diagnosis_m
  !! Define an abstraction for describing test outcomes and diagnostic information
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: test_diagnosis_t
  public :: assertion_diagnosis_t
  public :: call_julienne_assert
  public :: operator(.all.)
  public :: operator(.and.)
  public :: operator(.approximates.)
  public :: operator(.within.)
  public :: operator(.withinFraction.)
  public :: operator(.withinPercentage.)
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

  type, extends(test_diagnosis_t) :: assertion_diagnosis_t
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

  interface assertion_diagnosis_t

    pure module function construct_from_components(success, diagnostics_string) result(assertion_diagnosis)
      implicit none
      logical, intent(in) :: success
      character(len=*), intent(in) :: diagnostics_string
      type(assertion_diagnosis_t) assertion_diagnosis
    end function

    pure module function construct_with_string(success, diagnostics_string) result(assertion_diagnosis)
      implicit none
      logical, intent(in) :: success
      type(string_t), intent(in) :: diagnostics_string
      type(assertion_diagnosis_t) assertion_diagnosis
    end function

  end interface

  interface call_julienne_assert

    pure module subroutine julienne_assert(test_diagnosis, file, line)
      implicit none
      type(test_diagnosis_t), intent(in) :: test_diagnosis
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
    end subroutine

  end interface

  interface operator(.all.)
     
    pure module function aggregate_diagnosis(diagnoses) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: diagnoses(..)
      type(test_diagnosis_t) diagnosis
    end function

  end interface

  interface operator(.and.)
     
    elemental module function and(lhs, rhs) result(diagnosis)
      implicit none
      type(test_diagnosis_t), intent(in) :: lhs, rhs
      type(test_diagnosis_t) diagnosis
    end function

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

  interface operator(.equalsExpected.)

    elemental module function equals_expected_integer(actual, expected) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected
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

  end interface

  interface operator(.lessThanOrEqualTo.)

    elemental module function less_than_or_equal_to_integer(actual, expected_max) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_max
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

  interface operator(.greaterThanOrEqualTo.)

    elemental module function greater_than_or_equal_to_integer(actual, expected_min) result(test_diagnosis)
      implicit none
      integer, intent(in) :: actual, expected_min
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
