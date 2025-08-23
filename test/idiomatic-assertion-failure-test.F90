! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

program test_julienne_assert_intentional_failure
  !! Conditionally test an assertion that is hardwired to fail.
  use julienne_m, only : call_julienne_assert_, command_line_t, operator(.equalsExpected.)
  implicit none

  associate(command_line => command_line_t())
    if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
#ifdef RUN_FALSE_ASSERTIONS
      print '(a)', new_line('') // 'Test the intentional failure of an idiomatic assertion: ' // new_line('')
      call_julienne_assert(1 .equalsExpected. 2)
#else
      print *
      print '(a)', 'Skipping the test in ' // __FILE__ // '.'
      print '(a)', 'Add the following to your fpm command to test assertion failures: --flag "-DASSERTIONS -DRUN_FALSE_ASSERTIONS"'
      print *
#endif
    end if
  end associate

end program

