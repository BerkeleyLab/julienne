! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#if defined(__flang__)
  #define NO_MULTI_IMAGE_SUPPORT
#endif

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

  integer :: passes=0, tests=0

  block 
    type(command_line_t) command_line
    character(len=*), parameter :: usage = &
      new_line('') // new_line('') // &
      'Usage: fpm test -- [--help] | [--contains <substring>]' // &
      new_line('') // new_line('') // &
      'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
      'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
      'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('') 
    if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage
  end block

  call bin_test%report(passes, tests)
  call formats_test%report(passes, tests)
  call string_test%report(passes, tests)
  call test_result_test%report(passes, tests)
  call test_description_test%report(passes, tests)
  call vector_test_description_test%report(passes,tests)
  if (.not. GitHub_CI())  call command_line_test%report(passes, tests)

#ifndef NO_MULTI_IMAGE_SUPPORT
  if (this_image()==1) &
#endif
  print *
  print '(*(a,:,i))', "_________ In total, ",passes," of ",tests, " tests pass. _________"
  if (passes /= tests) error stop

end program
