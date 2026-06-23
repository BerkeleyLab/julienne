! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "language-support.F90"

program stop_and_print_in_pure_procedure
  !! Conditionally test printing via error termination inside a pure procedure
  use julienne_m, only : command_line_t, string_t, operator(.csv.), stop_and_print
  implicit none

#if HAVE_MULTI_IMAGE_SUPPORT
  associate(command_line => command_line_t(), me => this_image())
#else
  associate(command_line => command_line_t(), me => 1)
#endif
    if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
#if TEST_INTENTIONAL_FAILURE && ASSERTIONS
      if (me==1) print '(a)', new_line('') // 'Test the intentional failure of stop_and_print in a pure procedure: ' // new_line('')
      call pure_subroutine
#else
      if (me==1) print '(a)',  &
           new_line('') // &
           'Skipping the test in ' // __FILE__ // '.' // new_line('') // &
           'Add the following to your fpm command to test the stop_and_print: --flag "-DASSERTIONS -DTEST_INTENTIONAL_FAILURE"' // &
           new_line('')
#endif
    end if
  end associate

contains

  pure subroutine pure_subroutine
    integer, parameter :: array(4) = [1,2,3,4]
    call stop_and_print("array = " // .csv. string_t(array))
  end subroutine

end program
