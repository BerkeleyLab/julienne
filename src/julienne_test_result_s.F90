! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_test_result_m) julienne_test_result_s
  use julienne_user_defined_collectives_m, only : co_all
  implicit none

contains

    module procedure construct_from_character
      test_result%description_ = description
      test_result%passed_ = passed
    end procedure

    module procedure construct_from_string
      test_result%description_ = description
      test_result%passed_ = passed
    end procedure

    module procedure characterize
      characterization = trim(merge("passes on ", "FAILS on  ", self%passed_)) // " " // trim(self%description_%string()) // "."
    end procedure

    module procedure passed
      test_passed = self%passed_
#ifndef __flang__
      call co_all(test_passed)
#endif
    end procedure

    module procedure description_contains
      substring_found = index(self%description_%string(), substring%string()) /= 0
    end procedure

end submodule julienne_test_result_s
