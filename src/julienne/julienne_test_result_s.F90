! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "language-support.F90"

submodule(julienne_test_result_m) julienne_test_result_s
#if ASSERTIONS
  use julienne_assert_m, only : call_julienne_assert_
#endif
  implicit none

contains

    module procedure construct_from_string
      test_result%description_ = description
      if (present(diagnosis)) test_result%diagnosis_ = diagnosis
    end procedure

    module procedure construct_from_character
      test_result%description_ = description
      if (present(diagnosis)) test_result%diagnosis_ = diagnosis
    end procedure

#if HAVE_MULTI_IMAGE_SUPPORT

    module procedure co_characterize

      logical i_skipped, i_passed
      integer, parameter :: skips=1, passes=2
      integer tally(skips:passes)
      character(len=*), parameter :: indent = "   "

      i_skipped = .not. allocated(self%diagnosis_)

      if (i_skipped) then
        i_passed = .false.
      else
        i_passed = self%diagnosis_%test_passed()
      end if
      
      tally = [merge(1,0,i_skipped), merge(1,0,i_passed)]
      call co_sum(tally)
      
      associate(me => this_image(), images => num_images(), images_skipped => tally(skips), images_passed => tally(passes))
        call_julienne_assert(any(images_skipped == [0,images]))
        if (i_skipped) then
          if (me==1) print '(a)', indent // "SKIPS  on " // trim(self%description_%string()) // "."
        else
          if (me==1) print '(a)', indent // merge("passes on ", "FAILS  on ", images_passed == images) // trim(self%description_%string()) // "."
#if ! ASYNCHRONOUS_DIAGNOSTICS
          sync all
#endif
          if ((.not. i_skipped) .and. (.not. i_passed)) then
            associate(image => string_t(me))
              print '(a)', indent // indent // "diagnostics on image " // image%string() // ": " // self%diagnosis_%diagnostics_string()
            end associate
          end if
#if ! ASYNCHRONOUS_DIAGNOSTICS
          sync all
#endif
        end if
      end associate
    end procedure

#else

    module procedure co_characterize

      character(len=*), parameter :: indent = "   "

      if (.not. allocated(self%diagnosis_)) then
        print '(a)', indent // "SKIPS  on " // trim(self%description_%string()) // "."
      else
        associate(test_passed => self%diagnosis_%test_passed())
          print '(a)', indent // merge("passes on ", "FAILS  on ", test_passed) // trim(self%description_%string()) // "."
          if (.not. test_passed) print '(a)', indent //indent // "diagnostics: " // self%diagnosis_%diagnostics_string()
        end associate
      end if
    end procedure
#endif

    module procedure passed
      if (.not. allocated(self%diagnosis_)) then
        test_passed = .false.
      else
        test_passed = self%diagnosis_%test_passed()
      end if
    end procedure

    module procedure skipped
      test_skipped = .not. allocated(self%diagnosis_)
    end procedure

    module procedure description_contains_string
      substring_found = self%description_contains_characters(substring%string())
    end procedure

    module procedure description_contains_characters
      substring_found = index(self%description_%string(), substring) /= 0
    end procedure

end submodule julienne_test_result_s
