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
  subroutine stop_if_compiler_too_old
    character(len=:), allocatable :: compiler_identity
    integer major, minor
    compiler_identity = compiler_version()
    if (index(compiler_identity, "GCC")==1) then
      associate(               final_dot => index(compiler_identity                     ,"." ,back=.true.))
        associate(       penultimate_dot => index(compiler_identity(:final_dot-1)       ,"." ,back=.true.))
          associate(space_before_version => index(compiler_identity(:penultimate_dot-1) ," " ,back=.true.))
            associate( &
               major_string => compiler_identity(space_before_version+1 : penultimate_dot-1) &
              ,minor_string => compiler_identity(     penultimate_dot+1 :       final_dot-1) &
            )
              read(major_string, '(i2)') major
              read(minor_string, '(i1)') minor
              if ((major < 14) .or. (major==14 .and. minor<3)) stop "'"// compiler_identity //"' too old: GCC >= 14.3.0 required"
            end associate
          end associate
        end associate
      end associate
    end if
  end subroutine
end program
