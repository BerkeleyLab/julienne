#include "assert_features.h"

program test_assert_subroutine_error_termination
  !! Test "assert" subroutine calls that are intended to error terminate
  use assert_m, only : assert
  implicit none
  
  integer exit_status

  print *
  print *,"The assert subroutine"

  ! TODO: The following is a HORRIBLY fragile test. 
  ! Specifically, it encodes a bunch of compiler-specific flags into an fpm command, 
  ! and if fpm fails for any unrelated reason (broken command, compile error, I/O error, etc)
  ! we will mistakenly interpret that as a passing test!

  call execute_command_line( &
#ifdef __GFORTRAN__
    command = "fpm run --example false-assertion --profile release --flag '-DASSERTIONS -ffree-line-length-0' > /dev/null 2>&1", &
#elif NAGFOR
    command = "fpm run --example false-assertion --compiler nagfor --flag '-DASSERTIONS -fpp' > /dev/null 2>&1", &
#elif __flang__
    command = "./test/assert/run-false-assertion.sh | fpm run --example check-exit-status", &
#elif __INTEL_COMPILER
    command = "./test/assert/run-false-assertion-intel.sh | fpm run --example check-exit-status", &
#elif _CRAYFTN
    command = "fpm run --example false-assertion --profile release --compiler crayftn.sh --flag '-DASSERTIONS' > /dev/null 2>&1", &
#else
    command = "echo 'example/false_assertion.F90: unsupported compiler' && exit 1", &
#endif
    wait = .true., &
    exitstat = exit_status &
  )
    
#if ASSERT_MULTI_IMAGE
  block
    logical error_termination

    error_termination = exit_status /=0
    call co_all(error_termination)
    if (this_image()==1) then
      if (error_termination) then
        print *,"  passes on error-terminating when assertion = .false."
      else 
        print *,"  FAILS to error-terminate when assertion = .false. (Yikes! Who designed this OS?)"
      end if
    end if
  end block
#else
    block
      integer unit
      open(newunit=unit, file="build/exit_status", status="old")
      read(unit,*) exit_status
      print *,trim(merge("passes","FAILS ",exit_status/=0)) // " on error-terminating when assertion = .false."
      close(unit)
    end block 
#endif

contains

  pure function and_operation(lhs,rhs) result(lhs_and_rhs)
    logical, intent(in) :: lhs, rhs
    logical lhs_and_rhs
    lhs_and_rhs = lhs .and. rhs
  end function

#if ASSERT_MULTI_IMAGE
  subroutine co_all(boolean)
    logical, intent(inout) :: boolean
    call co_reduce(boolean, and_operation)
  end subroutine
#endif

end program test_assert_subroutine_error_termination
