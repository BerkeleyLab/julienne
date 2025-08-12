! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module julienne_test_m
  !! Define an abstract test_t type with deferred bindings ("subject" and "results")
  !! used by a type-bound procedure ("report") for reporting test results.  The "report"
  !! procedure thus represents an implementation of the Template Method pattern.
  use julienne_test_result_m, only : test_result_t
  use julienne_test_description_m, only : test_description_t, filter
  use julienne_user_defined_collectives_m, only : co_all
  use julienne_command_line_m, only : command_line_t

  implicit none

  private
  public :: test_t

  type, abstract :: test_t
    !! Facilitate testing and test reporting
  contains
    procedure(subject_interface), nopass, deferred :: subject 
    procedure(results_interface), nopass, deferred :: results
    procedure :: run
    procedure :: report
  end type

  abstract interface

    pure function subject_interface() result(specimen_description)
      !! The result is the name of the test specimen (the subject of testing)
      character(len=:), allocatable :: specimen_description
    end function

    function results_interface() result(test_results)
      !! The result is an array of test results for subsequent reporting in the "report" type-bound procedure
      import test_result_t
      type(test_result_t), allocatable :: test_results(:)
    end function

  end interface

  interface

    module function run(test, test_descriptions) result(test_results)
      !! Construct an array of test results from a set of tests filtered for descriptions and subjects with
      !! the '--contains' flag's value if the flag was included on the command line at program launch.
      implicit none
      class(test_t), intent(in) :: test
      type(test_description_t), intent(in) :: test_descriptions(:)
      type(test_result_t), allocatable :: test_results(:)
    end function

    module subroutine report(test, passes, tests, skips)
      !! Print the test results and increment the tallies of passing tests, total tests, and skipped tests.
      implicit none
      class(test_t), intent(in) :: test
      integer, intent(inout) :: passes, tests, skips
    end subroutine

  end interface

end module julienne_test_m