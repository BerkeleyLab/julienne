! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  use julienne_m, only : test_fixture_t, test_harness_t
  use specimen_test_m, only : specimen_test_t
  use widget_test_m, only : widget_test_t
  implicit none

  associate(test_harness => test_harness_t([ &
     test_fixture_t(specimen_test_t()) &
    ,test_fixture_t(widget_test_t()) &
  ]))
    call test_harness%report_results
  end associate
contains
#if __GNUC__ && ( __GNUC__ < 14 || (__GNUC__ == 14 && __GNUC_MINOR__ < 3) )
  stop "GFortran " // __VERSION__ // " too old: GCC >= 14.3.0 required"
#endif
end program test_suite_driver
