! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

program main
  !! Julienne unit tests driver

  ! Internal utilities
  use julienne_m                     ,only : command_line_t, GitHub_CI

  ! Test modules
  use bin_test_m                     ,only :                     bin_test_t
  use command_line_test_m            ,only :            command_line_test_t
  use formats_test_m                 ,only :                 formats_test_t
  use string_test_m                  ,only :                  string_test_t
  use test_result_test_m             ,only :             test_result_test_t
  use test_description_test_m        ,only :        test_description_test_t
  use vector_test_description_test_m ,only : vector_test_description_test_t
  implicit none

  type(bin_test_t) bin_test
  type(command_line_test_t) command_line_test
  type(formats_test_t) formats_test
  type(string_test_t) string_test
  type(test_result_test_t) test_result_test
  type(test_description_test_t) test_description_test
  type(vector_test_description_test_t) vector_test_description_test

  type(command_line_t) command_line

  integer :: passes=0, tests=0, skips=0

  character(len=*), parameter :: usage = &
    new_line('') // new_line('') // &
    'Usage: fpm test -- [--help] | [--contains <substring>]' // &
    new_line('') // new_line('') // &
    'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
    'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
    'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('')

  if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage

  print "(a)", new_line("") // "Append '-- --help' or '-- -h' to your `fpm test` command to display usage information."

  call bin_test%report(passes, tests, skips)
  call formats_test%report(passes, tests, skips)
  call string_test%report(passes, tests, skips)
  call test_result_test%report(passes, tests, skips)
  call test_description_test%report(passes, tests, skips)
  call vector_test_description_test%report(passes,tests, skips)

  if (.not. GitHub_CI())  then
    if (command_line%argument_present(["--test"])) then
      call command_line_test%report(passes, tests, skips)
    else
      write(*,"(a)")  &
      new_line("") // &
      "To also test Julienne's command_line_t type, append the following to your fpm test command:" // &
      new_line("") // &
      "-- --test command_line_t --type"
    end if
  end if

#if HAVE_MULTI_IMAGE_SUPPORT
  if (this_image()==1) then
#endif

    print *
    print '(*(a,:,g0))', "_________ In total, ",passes," of ",tests, " tests pass.  ", skips, " tests were skipped. _________"

    if (passes + skips /= tests) error stop "Some executed tests failed."

#if HAVE_MULTI_IMAGE_SUPPORT
  end if
#endif

end program
