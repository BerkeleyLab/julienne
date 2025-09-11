! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_test_harness_m) julienne_test_harness_s
  use iso_fortran_env, only : int64, real64
  use julienne_command_line_m, only : command_line_t
  use julienne_string_m, only : string_t
  implicit none

contains

    module procedure component_constructor
      test_harness%test_fixture_ = test_fixtures
    end procedure

    module procedure report_results

      integer i, passes, tests, skips
      integer(int64) start_time, end_time, clock_rate

      passes=0; tests=0; skips=0

      call print_usage_info_and_stop_if_requested
      call system_clock(start_time, clock_rate)

      do i = 1, size(self%test_fixture_)
        call self%test_fixture_(i)%report(passes, tests, skips)
      end do

      call system_clock(end_time)

#if HAVE_MULTI_IMAGE_SUPPORT
      associate(me => this_image(), image_count => num_images())
#else
      associate(me => 1, image_count => 1)
#endif
        if (me==1) then
          print *, ""
          print *, "Test-suite execution time: ", real(end_time - start_time, real64)/real(clock_rate, real64), " seconds"
          print *, "Number of images: ", image_count
          print *, ""
          print *, "_____ ", passes, " of ", tests, " tests passed. ", skips, " tests were skipped _____"
        end if
        if (passes + skips /= tests .and. me==1) error stop "Some tests failed."
      end associate

    end procedure

    subroutine print_usage_info_and_stop_if_requested

      character(len=*), parameter :: usage = &
        new_line('') // new_line('') // &
        'Usage: fpm test -- [--help] | [--contains <substring>]' // &
        new_line('') // new_line('') // &
        'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
        'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
        'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('')

#if HAVE_MULTI_IMAGE_SUPPORT
      associate(me => this_image(), image_count => num_images())
#else
      associate(me => 1, image_count => 1)
#endif
        associate(command_line => command_line_t())

          if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
            if (me==1) print '(a)', usage
            stop
          end if

          if (me==1) print '(a)', new_line("") // "Append '-- --help' or '-- -h' to your `fpm test` command to display usage information."

        end associate
      end associate
    end subroutine

end submodule julienne_test_harness_s
