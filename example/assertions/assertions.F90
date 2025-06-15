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
  print '(a)', "Evaluating two true assertions and one false assertion that will initiate error termination."
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
