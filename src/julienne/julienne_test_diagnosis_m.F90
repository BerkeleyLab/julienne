! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_test_diagnosis_m
  !! Define an abstraction for describing test outcomes and diagnostic information
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: test_diagnosis_t

  type test_diagnosis_t
    !! Encapsulate test outcome and diagnostic information
    private
    logical test_passed_
    character(len=:), allocatable :: diagnostics_string_
  contains
    procedure test_passed
    procedure diagnostics_string
  end type

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
