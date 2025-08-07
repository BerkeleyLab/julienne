! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_assert_m) julienne_assert_s
  use assert_m, only : assert_always
  use julienne_m, only : string_t
  implicit none

contains

  module procedure julienne_assert
    character(len=:), allocatable :: diagnostics_string
    diagnostics_string =  test_diagnosis%diagnostics_string()
    if (present(file)) diagnostics_string = diagnostics_string // " in file " // file
    if (present(line)) diagnostics_string = diagnostics_string // " at line " // string_t(line)
    call assert_always(test_diagnosis%test_passed(), diagnostics_string)
  end procedure

end submodule julienne_assert_s
