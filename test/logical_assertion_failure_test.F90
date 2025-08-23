! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

program logical_assertion_failure_test
  !! Conditionally test an assertion that is hardwired to fail.
  use julienne_m, only : call_julienne_assert_, command_line_t, operator(.equalsExpected.)
  implicit none
  integer, allocatable :: array(:)

  associate(command_line => command_line_t())
    if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
#ifdef RUN_FALSE_ASSERTIONS
      print '(a)', new_line('') // 'Test the intentional failure of a logical assertion: ' // new_line('')
      if (allocated(array)) deallocate(array)
      call_julienne_assert(allocated(array))
#else
      print *
      print '(a)', 'Skipping the test in ' // __FILE__ // '.'
      print '(a)', 'Add the following to your fpm command to test assertion failures: --flag "-DASSERTIONS -DRUN_FALSE_ASSERTIONS"'
      print *
#endif
    end if
  end associate

end program
