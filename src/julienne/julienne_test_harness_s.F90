! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_test_harness_m) julienne_test_harness_s
  use iso_fortran_env, only : int32, real32
  use julienne_command_line_m, only : command_line_t
  use julienne_string_m, only : string_t
  use julienne_multi_image_m, only : internal_this_image, internal_num_images, internal_error_stop
  implicit none

contains

    module procedure component_constructor
#ifdef NAGFOR
      test_harness%test_fixture_ = test_fixtures ! avoid a nagfor internal compiler error
#else
      allocate(test_harness%test_fixture_, source = test_fixtures) ! eliminates a harmless gfortran warning
#endif
    end procedure

    module procedure report_results

      integer i, passes, tests, skips
      integer(int32) start_time, end_time, clock_rate

      passes=0; tests=0; skips=0

      call print_usage_info_and_stop_if_requested
      call system_clock(start_time, clock_rate)

      do i = 1, size(self%test_fixture_)
        call self%test_fixture_(i)%report(passes, tests, skips)
      end do

      call do_random_time_consuming_stuff

      call system_clock(end_time)

      associate(me => internal_this_image(), image_count => internal_num_images())
        if (me==1) then
          print *
          print '(a,f0.3,a)', "Test-suite run time: ", (end_time - start_time)/(real(clock_rate, real32)), " seconds"
          print '(a,i0)',      "Number of images: ", image_count
          print *
          print '(*(a,:,i0))', "_____ ", passes, " of ", tests, " tests passed. ", skips, " tests were skipped _____"
          print *
        end if
        if (passes + skips /= tests .and. me==1) call internal_error_stop("Some tests failed.")
      end associate

    contains

      subroutine do_random_time_consuming_stuff
        use iso_fortran_env, only : int64, real64
        implicit none
        integer(int64) start_time, end_time, clock_rate, i
        integer, parameter :: N = 256 
        real(real64) A(N,N), B(N,N), C(N,N)
   
        call random_init(repeatable=.true., image_distinct=.true.)
        call random_number(A)
        call random_number(B)
        call random_number(C)

        do i = 1, 1000
          A = matmul(B,C)
        end do

        print *,"maxval(A) = ", maxval(A)

      end subroutine


    end procedure

    subroutine print_usage_info_and_stop_if_requested

      character(len=*), parameter :: usage = &
        new_line('') // new_line('') // &
        'Usage: fpm test -- [--help] | [--contains <substring>]' // &
        new_line('') // new_line('') // &
        'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
        'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
        'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('')

      associate(me => internal_this_image())
        associate(command_line => command_line_t())

          if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
            if (me==1) print '(a)', usage
            stop
          end if

          if (me==1) then

            print '(a)', new_line("") // "Append '-- --help' or '-- -h' to your `fpm test` command to display usage information."

#if (! defined(__GFORTRAN__)) && (! defined(NAGFOR))
            associate(search_string => command_line%flag_value("--contains"))
#else
            block; character(len=:), allocatable :: search_string; search_string = command_line%flag_value("--contains")
#endif
              if (len(search_string)==0) then
                print '(a)', new_line('') // &
                  "Running all tests." // new_line('') // &
                  "(Add '-- --contains <string>' to run only tests with subjects or descriptions containing the specified string.)"
              else
                print '(a)', new_line('') // "Running only tests with subjects or descriptions containing '" // search_string // "'."
              end if
#if (! defined(__GFORTRAN__)) && (! defined(NAGFOR))
            end associate
#else
            end block
#endif
          end if
        end associate
      end associate
    end subroutine

end submodule julienne_test_harness_s
