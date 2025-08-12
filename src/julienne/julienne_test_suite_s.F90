! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(julienne_test_suite_m) julienne_test_suite_s
  use assert_m
  use julienne_m, only : operator(.csv.)
  implicit none

  character(len=*), parameter :: test_suite_key = "test suite"
  character(len=*), parameter :: test_subjects_key = "test subjects"
  character(len=*), parameter :: copyright_and_license = &
       "! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute" // new_line('') &
    // "! Terms of use are as specified in LICENSE.txt"

contains

  module procedure from_components
    test_suite%test_subjects_ = test_subjects
  end procedure

  module procedure from_file
    integer l
    logical test_suite_key_found

    test_suite_key_found = .false.

    associate(lines => file%lines())
      do l=1,size(lines)
        if (lines(l)%get_json_key() == test_suite_key) then
          test_suite_key_found = .true.
          test_suite%test_subjects_ = lines(l+1)%get_json_value(string_t(test_subjects_key), mold=[string_t("")])
          return
        end if
      end do
    end associate

    call_assert(test_suite_key_found)
  end procedure

  module procedure to_file
    character(len=*), parameter :: indent = repeat(" ",ncopies=4)

    file = file_t([  &
       string_t("{") &
      ,string_t(indent // '"' // test_suite_key//  '": {') & 
      ,         indent // indent // '"' // test_subjects_key // '" : [' // .csv. self%test_subjects_%bracket('"')  // '],' &
      ,string_t(indent // '}') & 
      ,string_t("}") &
    ])  
  end procedure

  module procedure driver_file
    integer i

    type(string_t), allocatable :: test_types(:), test_modules(:)

    test_types   = self%test_subjects_ // "_test_t" ! Using GCC 14.3 or higher would allow this to be an association
    test_modules = self%test_subjects_ // "_test_m" ! Using GCC 14.3 or higher would allow this to be an association

    file = file_t([                                                                &
       string_t(copyright_and_license) // new_line('')                             &
      ,string_t(  "program test_suite_driver")                                     &
      ,string_t(  "  use julienne_m, only : test_fixture_t, test_harness_t")       &
      ,[(string_t("  use ") // test_modules(i) // string_t(", only : ") // test_types(i), i=1, size(test_modules))] &
      ,string_t(  "  implicit none") // new_line('')                               &
      ,string_t(  "  associate(test_harness => test_harness_t([ &"               ) &
      ,[(string_t("     test_fixture_t(") // test_types(1) // string_t("()) &"))] &
      ,[(string_t("    ,test_fixture_t(") // test_types(i) // string_t("()) &"), i=2, size(test_types  ))] &
      ,string_t(  "  ]))"                                                        ) &
      ,string_t(  "    call test_harness%report_results"                         ) &
      ,string_t(  "  end associate"                                              ) &
      ,string_t(  "end program test_suite_driver")                                 &
    ])
  end procedure

end submodule julienne_test_suite_s
