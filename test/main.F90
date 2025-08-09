! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  !! Julienne test-suite driver
  use julienne_m, only : test_fixture_t, test_harness_t, command_line_t, GitHub_CI

  ! Test modules:
  use assert_test_m                  ,only :                  assert_test_t
  use bin_test_m                     ,only :                     bin_test_t
  use command_line_test_m            ,only :            command_line_test_t
  use formats_test_m                 ,only :                 formats_test_t
  use string_test_m                  ,only :                  string_test_t
  use test_description_test_m        ,only :        test_description_test_t
  use test_diagnosis_test_m          ,only :          test_diagnosis_test_t
  use test_result_test_m             ,only :             test_result_test_t
  use vector_test_description_test_m ,only : vector_test_description_test_t

  implicit none

  type(test_harness_t) test_harness
  integer :: passes=0, tests=0, skips=0

  ! Construct test harness from an array of test fixtures, each
  ! of which is constructed from a test structure constructors:
  associate(test_harness => test_harness_t([          &
     test_fixture_t(                 assert_test_t()) &
    ,test_fixture_t(                    bin_test_t()) &
    ,test_fixture_t(                formats_test_t()) &
    ,test_fixture_t(                 string_test_t()) &
    ,test_fixture_t(       test_description_test_t()) &
    ,test_fixture_t(         test_diagnosis_test_t()) &
    ,test_fixture_t(            test_result_test_t()) &
    ,test_fixture_t(vector_test_description_test_t()) &
  ]))
    call test_harness%report(passes, tests, skips)
  end associate

  if (.not. GitHub_CI())  then
    block
      type(command_line_t) command_line
      type(command_line_test_t) command_line_test
     
      if (command_line%argument_present(["--test"])) then
        call command_line_test%report(passes, tests, skips)
      else
        write(*,"(a)")  &
        new_line("") // &
        "To also test Julienne's command_line_t type, append the following to your fpm test command:" // &
        new_line("") // &
        "-- --test command_line_t --type"
      end if
    end block
  end if

  print *
  print '(*(a,:,g0))', "_________ In total, ",passes," of ",tests, " tests pass.  ", skips, " tests were skipped. _________"

  if (passes + skips /= tests) error stop "Some executed tests failed."

end program
