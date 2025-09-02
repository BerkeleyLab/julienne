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

  module procedure test_subjects
    subjects = self%test_subjects_
  end procedure

  module procedure test_modules
    modules = self%test_subjects_ // "_test_m"
  end procedure

  module procedure test_types
    types  = self%test_subjects_ // "_test_t"
  end procedure


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

    test_types   = self%test_types()   ! GCC 14.2 blocks the use of an association
    test_modules = self%test_modules() ! GCC 14.2 blocks the use of an association

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

  module procedure stub_file

    character(len=:), allocatable :: subject_module, subject_type, test_module, test_type

    subject_module = subject // "_m"
    subject_type   = subject // "_t"
    test_module    = subject // "_test_m"
    test_type      = subject // "_test_t"

    file = file_t([ &
       string_t(copyright_and_license) // new_line('') &
      ,string_t("module ") // test_module &
      ,string_t("  use julienne_m, only : test_t, test_description_t, test_diagnosis_t, test_result_t")&
      ,string_t("  use julienne_m, only : operator(.approximates.), operator(.within.), operator(.all.), operator(//)")&
      ,string_t("  use " // subject_module // ", only : " // subject_type) &
      ,string_t("  implicit none") // new_line('') &
      ,string_t("  type, extends(test_t) :: ") // test_type &
      ,string_t("  contains") &
      ,string_t("    procedure, nopass :: subject") &
      ,string_t("    procedure, nopass :: results") &
      ,string_t("  end type") // new_line('') &
      ,string_t("contains") // new_line('') &
      ,string_t("  pure function subject() result(test_subject)") &
      ,string_t("    character(len=:), allocatable :: test_subject") &
      ,string_t("    test_subject = 'A ") // subject // "'" &
      ,string_t("  end function") // new_line('') &
      ,string_t("  function results() result(test_results)") &
      ,string_t("    type(") // test_type // ") " // subject // "_test" &
      ,string_t("    type(test_result_t), allocatable :: test_results(:)") &
      ,string_t("    test_results = ") // subject // "_test%run( & " &
      ,string_t("      [test_description_t('doing something', do_something) &") &
      ,string_t("      ,test_description_t('checking something', check_something) &") &
      ,string_t("      ,test_description_t('skipping something') &") &
      ,string_t("    ])") &
      ,string_t("  end function") // new_line('') &
      ,string_t("  function check_something() result(test_diagnosis)") &
      ,string_t("    type(test_diagnosis_t) test_diagnosis") &
      ,string_t("    type(") // subject_type // ") " // subject &
      ,string_t("    test_diagnosis = .all.([22./7., 3.14159] .approximates. ") // subject // "%pi() .within. 0.001) // ' (pi approximation)'" &
      ,string_t("  end function") // new_line('')  &
      ,string_t("  function do_something() result(test_diagnosis)") &
      ,string_t("    type(test_diagnosis_t) test_diagnosis") &
      ,string_t("    test_diagnosis = test_diagnosis_t(test_passed = 1 == 1, diagnostics_string = 'craziness ensued')") &
      ,string_t("  end function") // new_line('') &
      ,string_t("end module") &
    ])
  end procedure

  module procedure write_driver
    integer file_unit, l
    type(string_t) use_statement, fixture_constructor
    type(string_t), allocatable :: test_modules(:), test_types(:)

    open(newunit=file_unit, file=file_name, form='formatted', status='unknown', action='write')

    write(file_unit, '(a)') copyright_and_license // new_line('')
    write(file_unit, '(a)') "program test_suite_driver"
    write(file_unit, '(a)') "  use julienne_m, only : test_fixture_t, test_harness_t"

    block
      type(string_t), allocatable :: test_modules(:)
      type(string_t) use_statement
      test_modules = self%test_modules() ! GCC 14.2 blocks the use of an association
      test_types = self%test_types()     ! GCC 14.2 blocks the use of an association
      do l = 1, size(test_modules)
        use_statement =  "  use " // test_modules(l) // ", only : " // test_types(l)
        write(file_unit, '(a)')  use_statement%string()
      end do
    end block

    write(file_unit, '(a)') "  implicit none" // new_line('')
    write(file_unit, '(a)') "  associate(test_harness => test_harness_t([ &"

    block
      type(string_t), allocatable :: test_types(:)
      type(string_t) fixture_constructor
      test_types   = self%test_types()   ! GCC 14.2 blocks the use of an association
      fixture_constructor =  "     test_fixture_t(" // test_types(1) // "()) &"
      write(file_unit, '(a)')  fixture_constructor %string()
      do l = 2, size(test_modules)
        fixture_constructor= "    ,test_fixture_t(" // test_types(l) // "()) &"
        write(file_unit, '(a)')  fixture_constructor%string()
      end do
    end block

    write(file_unit, '(a)') "  ]))"
    write(file_unit, '(a)') "    call test_harness%report_results"
    write(file_unit, '(a)') "  end associate"
    write(file_unit, '(a)') "end program test_suite_driver"

    close(file_unit)
  end procedure

end submodule julienne_test_suite_s
