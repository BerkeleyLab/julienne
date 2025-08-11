! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  !! Julienne test-suite driver

#if ! defined(__GCC__) || (GCC_VERSION >= 140300)
  ! Test infrastructure:
  use julienne_m, only : test_fixture_t, test_harness_t

  ! Modules containing test_t child types:
  use assert_test_m                  ,only :                  assert_test_t
  use bin_test_m                     ,only :                     bin_test_t
  use command_line_test_m            ,only :            command_line_test_t
  use formats_test_m                 ,only :                 formats_test_t
  use string_test_m                  ,only :                  string_test_t
  use test_description_test_m        ,only :        test_description_test_t
  use test_diagnosis_test_m          ,only :          test_diagnosis_test_t
  use test_result_test_m             ,only :             test_result_test_t

  implicit none

  ! Construct a test harness from an array of test fixtures, each of which is 
  ! constructed from an invocation of a test_t child type's structure constructor:
  associate(test_harness => test_harness_t([          &
     test_fixture_t(                 assert_test_t()) &
    ,test_fixture_t(                    bin_test_t()) &
    ,test_fixture_t(                formats_test_t()) &
    ,test_fixture_t(                 string_test_t()) &
    ,test_fixture_t(       test_description_test_t()) &
    ,test_fixture_t(         test_diagnosis_test_t()) &
    ,test_fixture_t(            test_result_test_t()) &
    ,test_fixture_t(           command_line_test_t()) &
  ]))
    call test_harness%report_results
  end associate
#endif
end program
