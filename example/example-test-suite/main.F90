! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

program main
  !! Example test main program to demonstrate the printing of diagnostic output when a test fails
  use julienne_m, only : command_line_t
  use specimen_test_m, only : specimen_test_t
  implicit none

  type(specimen_test_t) specimen_test
  integer :: passes=0, tests=0, skips=0

  call print_usage_and_stop_if_help_requested
  call specimen_test%report(passes, tests, skips)
  call report_tally_and_error_stop_if_test_fails

contains

  subroutine print_usage_and_stop_if_help_requested
    type(command_line_t) command_line
    if (command_line%argument_present([character(len=len("--help"))::"--help","-h"]))  then
      print *
      print '(a)', 'Usage: fpm run --example main -- [--help] | [--contains <substring>]'
      print *
      print '(a)', 'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,'
      print '(a)', 'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to'
      print '(a)', 'the tests with test subjects or test descriptions containing the user-specified substring.'
      stop
    else
      print * 
      print "(a)", "Append '-- --help' or '-- -h' to your `fpm test` command to display usage information."
    end if
  end subroutine

  subroutine report_tally_and_error_stop_if_test_fails

#if HAVE_MULTI_IMAGE_SUPPORT
    if (this_image()==1) then
#endif
      print *
      print '(*(a,:,g0))', "_________ In total, ",passes," of ",tests, " tests pass. ", skips , " tests were skipped _________"
      if (passes /= tests) error stop "Some tests failed."
#if HAVE_MULTI_IMAGE_SUPPORT
    end if
#endif

  end subroutine

end program
