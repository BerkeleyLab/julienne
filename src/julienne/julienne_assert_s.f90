! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_assert_m) julienne_assert_s
  use assert_m, only : assert_always
  implicit none

contains

  module procedure julienne_assert
    if (.not. test_diagnosis%test_passed()) then
      associate(diagnostics_string => test_diagnosis%diagnostics_string())
        call assert_always(.false., diagnostics_string%string(), file, line)
      end associate
    end if
  end procedure

end submodule julienne_assert_s
