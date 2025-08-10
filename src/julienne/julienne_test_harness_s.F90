! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_test_harness_m) julienne_test_harness_s
  use julienne_command_line_m, only : command_line_t
  implicit none

contains

    module procedure component_constructor
      test_harness%test_fixture_ = test_fixtures
    end procedure

    module procedure report_results

      call print_usage_info_and_stop_if_requested

      block
        integer i, passes, tests, skips

        passes=0; tests=0; skips=0

        do i = 1, size(self%test_fixture_)
          call self%test_fixture_(i)%report(passes, tests, skips)
        end do

        print '(a,*(a,:,g0))', new_line(''), "_______ ", passes, " of ", tests, " tests pass. ", skips, " tests were skipped _______"

        if (passes + skips /= tests) error stop "Some tests failed."
      end block

    end procedure

    subroutine print_usage_info_and_stop_if_requested

      associate(command_line => command_line_t())
        block
          character(len=*), parameter :: usage = &
            new_line('') // new_line('') // &
            'Usage: fpm test -- [--help] | [--contains <substring>]' // &
            new_line('') // new_line('') // &
            'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
            'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
            'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('')

          if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage
        end block
      end associate

      print "(a)", new_line("") // "Append '-- --help' or '-- -h' to your `fpm test` command to display usage information."

    end subroutine

end submodule julienne_test_harness_s