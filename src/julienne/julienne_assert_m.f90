! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_assert_m
  !! Define interfaces for writing assertions
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  private
  public :: call_julienne_assert_
  public :: julienne_assert

  interface call_julienne_assert_

    pure module subroutine julienne_assert(test_diagnosis, file, line)
      !! Public procedure.
      !! Use cases:
      !!   1. When invoked via the generic interface, the preprocessor passes the 'file' and 'line' dummy arguments automatically.
      !!   2. When invoked directly, there is 1 argument: an expression containing defined operations such as 1 .equalsExpected. 1
      implicit none
      type(test_diagnosis_t), intent(in) :: test_diagnosis
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
    end subroutine

    pure module subroutine assert_assert(assertion, file, line)
      !! Private wrapper for the assert subroutine in the Assert library (https://go.lbl.gov/assert).
      !! Invoke this procedure via the call_julienne_assert_ generic interface with only the required logical argument.
      !! The preprocessor will insert the optional 'file' and 'line' arguments.
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
    end subroutine

  end interface

end module julienne_assert_m
