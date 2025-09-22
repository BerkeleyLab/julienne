! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "language-support.F90"

submodule(julienne_test_result_m) julienne_test_result_s
#if HAVE_MULTI_IMAGE_SUPPORT
  use julienne_user_defined_collectives_m, only : co_all
#endif
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

    module procedure characterize

      logical test_skipped, test_passed
      character(len=*), parameter :: indent = "   "

      test_skipped = .not. allocated(self%diagnosis_)

      associate(me => this_image())
        if (test_skipped) then
          call co_all(test_skipped)
          call_julienne_assert(test_skipped)
          if (me==1) print '(a)', indent // "SKIPS  on " // trim(self%description_%string()) // "."
        else
          test_passed = self%diagnosis_%test_passed()
          if (me==1) print '(a)', indent // merge("passes on ", "FAILS  on ", test_passed) // trim(self%description_%string()) // "."
#if ! ASYNCHRONOUS_DIAGNOSTICS
          sync all
#endif
          associate(image => string_t(me))
            if (.not. test_passed) print '(a)', indent // indent // "diagnostics on image " // image%string() // ": " // self%diagnosis_%diagnostics_string()
          end associate
#if ! ASYNCHRONOUS_DIAGNOSTICS
          sync all
#endif
        end if
      end associate
    end procedure

#else

    module procedure characterize

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
