! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_assert_m) julienne_assert_s
  use assert_m, only : assert_always
  implicit none

contains

  module procedure idiomatic_assert
    character(len=:), allocatable :: description_

    if (.not. test_diagnosis%test_passed()) then
      if (present(description)) then
        description_ =  new_line('') // description // new_line('')
      else
        description_ =  new_line('')
      end if
      associate(diagnostics_string => description_ // test_diagnosis%diagnostics_string())
        call assert_always(.false., diagnostics_string%string(), file, line)
      end associate
    end if

  end procedure

  module procedure logical_assert
    character(len=:), allocatable :: description_

    if (.not. assertion) then
      if (present(description)) then
        description_ =  new_line('') // description // new_line('')
      else
        description_ =  new_line('')
      end if
      call assert_always(.false., description_ , file, line)
    end if

  end procedure

end submodule julienne_assert_s
