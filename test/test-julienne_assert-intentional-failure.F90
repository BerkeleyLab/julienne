! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

program test_julienne_assert_intentional_failure
  !! Conditionally test an assertion that is hardwired to fail.

#ifdef RUN_FALSE_ASSERTIONS

  use julienne_m, only : operator(.equalsExpected.), call_julienne_assert_
  implicit none
  print '(a)', new_line('') // 'Test julienne_assert intentional failure: ' // new_line('')
  call_julienne_assert(1 .equalsExpected. 2)

#else

  print *
  print '(a)', 'Skipping the test in ' // __FILE__ // '.'
  print '(a)', 'Add the following to your fpm command to test assertion failure: --flag "-DASSERTIONS -DRUN_FALSE_ASSERTIONS"'
  print *

#endif

end program

