! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_test_m) julienne_test_s
  use julienne_test_description_m, only : filter
  implicit none

contains

#if __GNUC__ && ( __GNUC__ > 13)

  module procedure run
    associate(matching_descriptions => filter(test_descriptions, test%subject()))
      test_results = matching_descriptions%run()
    end associate
  end procedure

#else

  module procedure run
    type(test_description_t), allocatable :: matching_descriptions(:)
    matching_descriptions = filter(test_descriptions, test%subject())
    test_results = matching_descriptions%run()
  end procedure

#endif

  module procedure report

    integer t
    logical, allocatable :: passing_tests(:), skipped_tests(:)
    type(test_result_t), allocatable :: test_results(:)

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image())
#else
    associate(me => 1)
#endif
      if (me==1) print '(a)', new_line('') // test%subject()

      test_results = test%results()

      skipped_tests = test_results%skipped()

      associate(num_tests => size(test_results))

          do t = 1, num_tests
            call test_results(t)%co_characterize()
          end do
          passing_tests = test_results%passed() ! may be altered by co_characterize

          tests = tests + num_tests

          associate(num_passes => count(passing_tests), num_skipped => count(skipped_tests))
            if (me==1) print '(*(a,:,i0))', " ", num_passes, " of ", num_tests, " tests passed. ", num_skipped, " tests were skipped."
            passes = passes + num_passes
            skips  = skips  + num_skipped
          end associate
      end associate
    end associate
  end procedure

end submodule julienne_test_s
