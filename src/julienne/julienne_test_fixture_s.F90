! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_test_fixture_m) julienne_test_fixture_s
  implicit none

contains

  module procedure component_constructor
    test_fixture%test_ = test
  end procedure

  module procedure report
    call self%test_%report(passes, tests, skips)
  end procedure

end submodule julienne_test_fixture_s