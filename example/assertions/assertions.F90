! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

program assertions
  !! Example: two true assertions followed by one intentionally false assertion
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
    ,operator(.withinPercentage.)
  implicit none

#if ! ASSERTIONS
  print *
  print '(a)', "Skipping asertions."
  print '(a)', "Use a command such as the following to rerun with assertions:"
  print '(a)', "fpm run --example assertions --flag -DASSERTIONS"
#else
  print '(a)', new_line('')
  print '(a)', "This program evaluates the following true assertions that should run silently:" // new_line('')
  print '(a)', "   call_julienne_assert(pi_ .approximates. pi .within. absolute_tolerance)"
  print '(a)', "   call_julienne_assert(pi_ .approximates. pi .withinPercentage. relative_tolerance)" // new_line('')
  print '(a)', "where pi_ = 22./7., pi 3.14152654, absolute_tolerance = 0.1, and relative_tolerance = 1.0." // new_line('')
  print '(a)', "The program will then evaluate one false assertion:" // new_line('')
  print '(a)', "   call_julienne_assert(1 .equalsExpected. 2)" // new_line('')
  print '(a)', "which should initiate error termination and provide a diagnostic message:" // new_line('')
#endif

  block
    real, parameter :: pi_ = 22./7.
    real, parameter :: pi  = 3.141592654
    real, parameter :: absolute_tolerance = 0.2
    real, parameter :: relative_tolerance = 1.0 ! percentage

    call_julienne_assert(pi_ .approximates. pi .within. absolute_tolerance)
    call_julienne_assert(pi_ .approximates. pi .withinPercentage. relative_tolerance)
    call_julienne_assert(1 .equalsExpected. 2) ! intentional failure
  end block
end program
