!     (c) 2024-2025 UC Regents, see LICENSE file for detailed terms.
!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
#include "assert_macros.h"

module assert_subroutine_m
  !! summary: Utility for runtime enforcement of logical assertions.
  !! usage: error-terminate if the assertion fails:
  !!
  !!    use assertions_m, only : assert
  !!    call assert( 2 > 1, "2 > 1")
  !!
  !! Assertion enforcement is controlled via the `ASSERTIONS` preprocessor macro,
  !! which can be defined to non-zero or zero at compilation time to
  !! respectively enable or disable runtime assertion enforcement.
  !!
  !! When the `ASSERTIONS` preprocessor macro is not defined to any value,
  !! the default is that assertions are *disabled* and will not check the condition.
  !!
  !! Disabling assertion enforcement may eliminate any associated runtime
  !! overhead by enabling optimizing compilers to ignore the assertion procedure
  !! body during a dead-code-removal phase of optimization.
  !!
  !! To enable assertion enforcement (e.g., for a debug build), define the preprocessor ASSERTIONS to non-zero.
  !! This file's capitalized .F90 extension causes most Fortran compilers to preprocess this file so
  !! that building as follows enables assertion enforcement: 
  !!
  !!    fpm build --flag "-DASSERTIONS"
  !!
  implicit none
  private
  public :: assert, assert_always

#if ASSERT_PARALLEL_CALLBACKS
    public :: assert_this_image_interface, assert_this_image
    public :: assert_error_stop_interface, assert_error_stop

    abstract interface
      pure function assert_this_image_interface() result(this_image_id)
        implicit none
        integer :: this_image_id
      end function
    end interface
    procedure(assert_this_image_interface), pointer :: assert_this_image
    
    abstract interface
      pure subroutine assert_error_stop_interface(stop_code_char)
        implicit none
        character(len=*), intent(in) :: stop_code_char
      end subroutine
    end interface
    procedure(assert_error_stop_interface), pointer :: assert_error_stop

#endif

#ifndef USE_ASSERTIONS
#  if ASSERTIONS
#    define USE_ASSERTIONS .true.
#  else
#    define USE_ASSERTIONS .false.
#  endif
#endif
  logical, parameter :: enforce_assertions=USE_ASSERTIONS

  interface

    pure module subroutine assert(assertion, description, diagnostic_data)
      !! If assertion is .false. and enforcement is enabled (e.g. via -DASSERTIONS=1), 
      !! then error-terminate with a character stop code that contains diagnostic_data if present
      implicit none
      logical, intent(in) :: assertion
        !! Most assertions will be expressions such as i>0
      character(len=*), intent(in) :: description
        !! A brief statement of what is being asserted such as "i>0" or "positive i"
      class(*), intent(in), optional :: diagnostic_data
        !! Data to include in an error ouptput: may be of an intrinsic type or a type that extends characterizable_t
    end subroutine

    pure module subroutine assert_always(assertion, description, diagnostic_data)
      !! Same as above but always enforces the assertion (regardless of ASSERTIONS)
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
      class(*), intent(in), optional :: diagnostic_data
    end subroutine

  end interface

end module assert_subroutine_m
