! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_test_result_m) julienne_test_result_s
  use julienne_user_defined_collectives_m, only : co_all
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

    module procedure characterize
      if (.not. allocated(self%diagnosis_)) then
        characterization = "SKIPS  on " // trim(self%description_%string()) // "."
      else
        associate(test_passed => self%diagnosis_%test_passed())
          characterization = merge("passes on ", "FAILS  on ", test_passed) // trim(self%description_%string()) // "."
          if (.not. test_passed) &
            characterization = characterization // new_line('') // "      diagnostics: " // self%diagnosis_%diagnostics_string()
        end associate
      end if
    end procedure

    module procedure passed
      if (.not. allocated(self%diagnosis_)) then
        test_passed = .false.
      else
        test_passed = self%diagnosis_%test_passed()
      end if
      call co_all(test_passed)
    end procedure

    module procedure skipped
      test_skipped = merge(.false., .true., allocated(self%diagnosis_))
      call co_all(test_skipped)
    end procedure

    module procedure description_contains_string
      substring_found = self%description_contains_characters(substring%string())
    end procedure

    module procedure description_contains_characters
      substring_found = index(self%description_%string(), substring) /= 0
    end procedure

end submodule julienne_test_result_s
