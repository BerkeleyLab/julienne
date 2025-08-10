! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_test_harness_m
  !! Define a test harness encapsulating an array of text fixtures, each of which can run a set of tests.
  use julienne_test_fixture_m, only : test_fixture_t

  implicit none

  private
  public :: test_harness_t

  type test_harness_t
    !! Encapsulate a set of test fixtures, each of which can run a set of tests.
    private
    type(test_fixture_t), allocatable :: test_fixture_(:)
  contains
    procedure report
  end type

  interface test_harness_t

    module function component_constructor(test_fixtures) result(test_harness) ! can be pure in Fortran 2028
      !! Component-wise user-defined structure constructor
      class(test_fixture_t) test_fixtures(:)
      type(test_harness_t) test_harness
    end function

  end interface

  interface

    module subroutine report(self, passes, tests, skips)
      !! If command line includes -h or --help, print usage information and stop.
      !! Otherwise, run tests and print results, including diagnostics for any failures.
      !! Also, tally and print the numbers of passing tests, total tests, skipped tests.
      implicit none
      class(test_harness_t), intent(in) :: self
      integer, intent(inout) :: passes, tests, skips
    end subroutine

  end interface

end module julienne_test_harness_m