#include "assert_features.h"

program test_assert_subroutine_normal_termination
  !! Test direct calls to the "assert" subroutine that don't error-terminate
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

  print *
  print *,"The assert subroutine"

  call assert(assertion = .true., description = "3 keyword arguments ", diagnostic_data=0)
  call assert(            .true., description = "2 keyword arguments ", diagnostic_data=0)
  call assert(            .true.,               "1 keyword argument  ", diagnostic_data=0)
  call assert(            .true.,               "0 keyword arguments ",                 0)
  call assert(            .true.,               "no optional argument"                   )
#if ASSERT_MULTI_IMAGE
    sync all
    if (this_image()==1) &
#endif
    print *,"  passes on not error-terminating when assertion=.true. + combos of (non-)keyword and (non-)present optional arguments"

  
  array_1D_diagnostic_data: &
  block
    complex, parameter :: complex_1D(*) = [(1.,0.), (0.,1.)]
    integer, parameter :: integer_1D(*) = [1, 2]
    logical, parameter :: logical_1D(*) = [.true., .true.]
    real,    parameter ::    real_1D(*) = [1., 2.]

    call assert(all(abs(complex_1D) < 2.), "all(abs(complex_array) < 2.)", intrinsic_array_t(complex_1D))
    call assert(all(integer_1D      < 3 ), "all(int_array          < 3 )", intrinsic_array_t(integer_1D))
    call assert(all(logical_1D          ), "all(logical_array          )", intrinsic_array_t(logical_1D))
    call assert(all(real_1D         < 3.), "all(real_array         < 3.)", intrinsic_array_t(   real_1D))
#if ASSERT_MULTI_IMAGE
    sync all
    if (this_image()==1) &
#endif
    print *,"  passes on not error-terminating when diagnostic_data = intrinsic_array_t({complex|integer|logical|real} 1D arrays)"
  end block array_1D_diagnostic_data


end program test_assert_subroutine_normal_termination
