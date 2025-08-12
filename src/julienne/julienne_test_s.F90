! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_test_m) julienne_test_s
  implicit none

contains

  module procedure run
    associate(matching_descriptions => filter(test_descriptions, test%subject()))
      test_results = matching_descriptions%run()
    end associate
  end procedure

  module procedure report
    logical, save :: do_first_report = .true.

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image())
#else
    integer me
    me = 1
#endif
      image_1_prints_usage_info: &
      if (me==1) then
        block
          type(command_line_t) command_line
          first_report: &
          if (do_first_report) then
            do_first_report = .false.
            block
              character(len=:), allocatable :: search_string
              search_string = command_line%flag_value("--contains")
              if (len(search_string)==0) then
                print '(*(a))',           new_line('') // &
                  "Running all tests." // new_line('') // &
                  "(Add '-- --contains <string>' to run only tests with subjects or descriptions containing the specified string.)"
              else
                print '(*(a))',           new_line('') // &
                  "Running only tests with subjects or descriptions containing '", search_string, "'."
              end if
            end block
          end if first_report
        end block

        print '(*(a))', new_line(''), test%subject()

      end if image_1_prints_usage_info

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
              if (me==1) print '(a,3(i0,a))'," ",num_passes," of ", num_tests," tests passed. ", num_skipped, " tests were skipped."
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
            if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests passed."
            passes = passes + num_passes
          end associate
        end associate
      end block
#endif

    end associate

  end procedure

end submodule julienne_test_s
