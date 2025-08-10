! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_test_harness_m
  !! Define a wrapper type for the test_t type to facilitate creating a polymorphic 
  !! array of test_t objects.
  use julienne_test_fixture_m, only : test_fixture_t

  implicit none

  private
  public :: test_harness_t

  type test_harness_t
    private
    class(test_fixture_t), allocatable :: test_fixture_(:)
  contains
    procedure report
  end type

  interface test_harness_t

    module function component_constructor(test_fixtures) result(test_harness) ! can be pure in Fortran 2028
      !! Construct a test_harness_t object from its components
      class(test_fixture_t) test_fixtures(:)
      type(test_harness_t) test_harness
    end function

  end interface

  interface

    module subroutine report(self, passes, tests, skips)
      !! Print the test results and increment the tallies of passing tests, total tests, and skipped tests.
      implicit none
      class(test_harness_t), intent(in) :: self
      integer, intent(inout) :: passes, tests, skips
    end subroutine

  end interface

end module julienne_test_harness_m