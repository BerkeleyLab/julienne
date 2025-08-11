! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_test_m) julienne_test_s
  implicit none

contains

  module procedure report

    logical, save :: do_first_report = .true.
    type(command_line_t) command_line

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image())
#else
    integer me
    me = 1
#endif
      if (me==1) then

        first_report: &
        if (do_first_report) then
          do_first_report = .false.
          associate(search_string => command_line%flag_value("--contains"))
            if (len(search_string)==0) then
              print *
              print '(a)',"Running all tests."
              print '(a)',"(Add '-- --contains <string>' to run only tests with subjects or descriptions containing the specified string.)"
            else
              print '(*(a))',"Running only tests with subjects or descriptions containing '", search_string, "'."
            end if
          end associate
        end if first_report

        print '(*(a))', new_line('a'), test%subject()

      end if

#ifndef _CRAYFTN
      associate(test_results => test%results())
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            block
              integer i
              do i=1,num_tests
                if (me==1) print '(3x,a)', test_results(i)%characterize()
              end do
            end block
          end if
          block
            logical, allocatable :: passing_tests(:), skipped_tests(:)

            passing_tests = test_results%passed()
            skipped_tests = test_results%skipped()

            call co_all(passing_tests)
            call co_all(skipped_tests)

            associate(num_passes => count(passing_tests), num_skipped => count(skipped_tests))
              if (me==1) print '(a,3(i0,a))'," ",num_passes," of ", num_tests," tests pass.  ", num_skipped, " tests were skipped."
              passes = passes + num_passes
              skips  = skips  + num_skipped
            end associate
          end block
        end associate
#if HAVE_MULTI_IMAGE_SUPPORT
      end associate
#endif

#else
      block
        logical, allocatable :: passing_tests(:)
        type(test_result_t), allocatable :: test_results(:)
        integer i

        test_results = test%results()
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            do i=1,num_tests
              if (me==1) print '(3x,a)', test_results(i)%characterize()
            end do
          end if
          passing_tests = test_results%passed()
          call co_all(passing_tests)
          associate(num_passes => count(passing_tests))
            if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
            passes = passes + num_passes
          end associate
        end associate
      end block
#endif

    end associate

  end procedure

end submodule julienne_test_s
