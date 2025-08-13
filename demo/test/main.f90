! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  !! Example test-suite driver
  use julienne_m      ,only : test_fixture_t, test_harness_t ! Import test infrastructure
  use specimen_test_m ,only : specimen_test_t  ! Must be a non-abstract child type extending Julienne's test_t type
  use iso_fortran_env ,only : compiler_version
  implicit none

  call stop_if_compiler_too_old

  ! Construct a test harness from an array of test fixtures, each of which is
  ! constructed from a structure constructor for a type that extends test_t.
  associate(test_harness => test_harness_t( [ test_fixture_t(specimen_test_t()) ] ))
    call test_harness%report_results
  end associate

contains
#if __GNUC__ && ( __GNUC__ < 14 || (__GNUC__ == 14 && __GNUC_MINOR__ < 3) )
  stop "GFortran " // __VERSION__ // " too old: GCC >= 14.3.0 required"
#endif
end program
