! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "language-support.F90"

program idiomatic_assertion_failure_test
  !! Conditionally test an assertion that is hardwired to fail.
  use julienne_m, only : call_julienne_assert_, command_line_t, operator(.equalsExpected.), one_image_prints
  implicit none

#if HAVE_MULTI_IMAGE_SUPPORT
  associate(command_line => command_line_t(), me => this_image())
#else
  associate(command_line => command_line_t(), me => 1)
#endif
    if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
#ifdef TEST_INTENTIONAL_FAILURE
      if (me==1) print '(a)', new_line('') // 'Test the intentional failure of an idiomatic assertion: ' // new_line('')
      call_julienne_assert(1 .equalsExpected. 2)
#else
      if (me==1) print '(a)',  &
           new_line('') // &
           'Skipping the test in ' // __FILE__ // '.' // new_line('') // &
           'Add the following to your fpm command to test assertion failures: --flag "-DASSERTIONS -DTEST_INTENTIONAL_FAILURE"' // &
           new_line('')
#endif
    end if
  end associate

end program
