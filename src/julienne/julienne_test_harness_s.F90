! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_test_harness_m) julienne_test_harness_s
  implicit none

contains

    module procedure component_constructor
      test_harness%test_fixture_ = test_fixtures
    end procedure

    module procedure report
      integer i
      do i = 1, size(self%test_fixture_)
        call self%test_fixture_(i)%report(passes, tests, skips)
      end do
    end procedure

end submodule julienne_test_harness_s