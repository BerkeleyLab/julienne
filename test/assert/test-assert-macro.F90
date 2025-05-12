program test_assert_macros
  use assert_m
  implicit none

  print *
  print *,"The call_assert macro"

#undef ASSERTIONS
#define ASSERTIONS 1
#include "assert_macros.h"
  call_assert(1==1)
  print *,"  passes on not error-terminating when an assertion expression evaluating to .true. is the only argument"

#undef ASSERTIONS
#include "assert_macros.h"
  call_assert(.false.)
  print *,"  passes on being removed by the preprocessor when ASSERTIONS is undefined" // new_line('')

  !------------------------------------------

  print *,"The call_assert_describe macro"

#undef ASSERTIONS
#define ASSERTIONS 1
#include "assert_macros.h"
  call_assert_describe(.true., ".true.")
  print *,"  passes on not error-terminating when assertion = .true. and a description is present"

#undef ASSERTIONS
#include "assert_macros.h"
  call_assert_describe(.false., "")
  print *,"  passes on being removed by the preprocessor when ASSERTIONS is undefined" // new_line('')

  !------------------------------------------

  print *,"The call_assert_diagnose macro"

#undef ASSERTIONS
#define ASSERTIONS 1
#include "assert_macros.h"
  call_assert_diagnose(.true., ".true.", diagnostic_data=1)
  print *,"  passes on not error-terminating when assertion = .true. and description and diagnostic_data are present"

  block
  integer :: computed_checksum = 37, expected_checksum = 37

#if defined(_CRAYFTN)
  ! Cray Fortran uses different line continuations in macro invocations
  call_assert_diagnose( computed_checksum == expected_checksum, &
                      "Checksum mismatch failure!", &
                      expected_checksum )
  print *,"  passes with macro-style line breaks"

  call_assert_diagnose( computed_checksum == expected_checksum, & ! ensured since version 3.14
                        "Checksum mismatch failure!",           & ! TODO: write a better message here 
                        computed_checksum )
  print *,"  passes with C block comments embedded in macro"
#else
  call_assert_diagnose( computed_checksum == expected_checksum, \
                      "Checksum mismatch failure!", \
                      expected_checksum )
  print *,"  passes with macro-style line breaks"

  call_assert_diagnose( computed_checksum == expected_checksum, /* ensured since version 3.14 */ \
                        "Checksum mismatch failure!",           /* TODO: write a better message here */ \
                        computed_checksum )
  print *,"  passes with C block comments embedded in macro"
#endif

  end block

#undef ASSERTIONS
#include "assert_macros.h"
  call_assert_diagnose(.false., "", "")
  print *,"  passes on being removed by the preprocessor when ASSERTIONS is undefined" // new_line('')

  !------------------------------------------

#undef ASSERTIONS
#define ASSERTIONS 1
#include "assert_macros.h"
  print *,"The call_assert_* macros"
  block
    logical :: foo
    foo = check_assert(.true.)
    print *,"  pass on invocation from a pure function"
  end block

contains 

  pure function check_assert(cond) result(ok)
    logical, intent(in) :: cond
    logical ok

    call_assert(cond)
    call_assert_describe(cond, "check_assert")
    call_assert_diagnose(cond, "check_assert", "")

    ok = .true.
  end function

end program
