! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_test_fixture_m
  !! Define a wrapper type for the test_t type to facilitate creating a polymorphic 
  !! array of test_t objects.
  use julienne_test_m, only : test_t

  implicit none

  private
  public :: test_fixture_t

  type test_fixture_t
    class(test_t), allocatable :: test_
  contains
    procedure report
  end type

  interface test_fixture_t

    module function component_constructor(test) result(test_fixture) ! can be pure in Fortran 2023
      !! Construct a test_fixture_t object from its components
      implicit none
      class(test_t), intent(in), allocatable :: test
      type(test_fixture_t) test_fixture
    end function

  end interface

  interface

    module subroutine report(self, passes, tests, skips)
      !! Print the test results and increment the tallies of passing tests, total tests, and skipped tests.
      implicit none
      class(test_fixture_t), intent(in) :: self 
      integer, intent(inout) :: passes, tests, skips
    end subroutine

  end interface

end module julienne_test_fixture_m