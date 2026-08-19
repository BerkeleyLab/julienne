! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module file_test_m
  !! Check data partitioning across files
  use julienne_m, only : &
     file_t &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.equalsExpected.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher
  use assert_m, only : assert
  implicit none

  private
  public :: file_test_t

  type, extends(test_t) :: file_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A file_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(file_test_t) file_test

    test_descriptions = [ &
      test_description_t(string_t("reading a written file"), usher(check_write_then_read)) &
    ]
    test_results = file_test%run(test_descriptions)
  end function

  function check_write_then_read() result(test_diagnosis)
    !! Check that a written file can be read correctly
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: file_name = "build/file_t-unit-test-data.txt"

    test_diagnosis = passing_test()

    associate(output_lines => [string_t("foo"), string_t(""), string_t("bar ")])
      associate(output_file => file_t(output_lines))
        call output_file%write_lines(file_name)
        associate(input_file => file_t(file_name))
          test_diagnosis = test_diagnosis .also. (.all. (input_file%lines() .equalsExpected. output_lines))
        end associate
      end associate
    end associate

  end function

end module file_test_m
