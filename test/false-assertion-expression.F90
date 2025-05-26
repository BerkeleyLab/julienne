! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program false_assertion_expression
  !! Test an assertion that is hardwired to fail by directly calling julienne_assert
  !! with a false test_diagnosis_t expression.

#ifdef RUN_FALSE_ASSERTIONS

  use julienne_m, only : operator(.equalsExpected.), assert_expression
  implicit none

  call assert_expression(1 .equalsExpected. 2)

#endif

end program

