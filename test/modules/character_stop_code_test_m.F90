! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module character_stop_code_test_m
  !! Check data partitioning across bins
  use julienne_m, only : &
     character_stop_code &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher
  implicit none

  private
  public :: character_stop_code_test_t

  type, extends(test_t) :: character_stop_code_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The character_stop_code function"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(character_stop_code_test_t) character_stop_code_test

    test_descriptions = [ &
      test_description_t(string_t("converting a 1D array to a comma-separated-value string"), usher(check_1D_array)) &
    ]
    test_results = character_stop_code_test%run(test_descriptions)
  end function

  function check_1D_array() result(test_diagnosis)
    !! Check conversion of a 1D array to a character string containing comma-separated values
    type(test_diagnosis_t) test_diagnosis

    integer, parameter :: expected_array(*) = [1,2,3,4]
    integer actual_array(size(expected_array))


    test_diagnosis = passing_test()

    read(character_stop_code(expected_array),*) actual_array
    test_diagnosis = .all. (actual_array .equalsExpected. expected_array)
  end function

end module character_stop_code_test_m
