! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module character_stop_code_test_m
  !! Check data partitioning across bins
  use julienne_m, only : &
     character_stop_code &
    ,operator(//) &
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
  implicit none

  private
  public :: character_stop_code_test_t

  type, extends(test_t) :: character_stop_code_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  interface operator(.occurrencesIn.)
    module procedure occurrences_in
  end interface

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
       test_description_t(string_t("converting a 1D array to a comma-separated-value (CSV) string"), usher(check_1D_array)) &
      ,test_description_t(string_t("converting a 2D array to new-line-separated CSV strings"), usher(check_2D_array)) &
    ]
    test_results = character_stop_code_test%run(test_descriptions)
  end function

  pure function occurrences_in(lhs, rhs) result(occurrences)
    character(len=1), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    integer occurrences, c
    occurrences = count([(rhs(c:c)==lhs, c=1,len(rhs))])
  end function

  function search_and_replace(string, search_for, replace_with) result(replacement_string)
    character(len=*), intent(in) :: string
    character(len=1), intent(in) :: search_for, replace_with
    character(len=len(string)) :: replacement_string

    do concurrent(integer :: c = 1:len(string))
      replacement_string(c:c) = merge(string(c:c), replace_with, string(c:c)/=search_for)
    end do
  end function

  function check_1D_array() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_array(*) = [1,2,3,4]
    integer c, actual_array(size(expected_array))

    test_diagnosis = passing_test()

    associate(stop_code => character_stop_code(expected_array))
      read(stop_code,*) actual_array
      test_diagnosis = test_diagnosis .also. .all. (actual_array .equalsExpected. expected_array)
      test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. size(expected_array)-1) // " commas in " // stop_code
    end associate
  end function

  function check_2D_array() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_array(*,*) = reshape([1,2,3,4,5,6], [2,3])
    integer actual_array(size(expected_array,1),size(expected_array,2))

    test_diagnosis = passing_test()

    associate( &
       stop_code => character_stop_code(expected_array) &
      ,rows => size(expected_array,1) &
      ,cols => size(expected_array,2) &
    )
      read(stop_code,*) actual_array(1,:), actual_array(2,:)

      test_diagnosis = test_diagnosis .also. .all. (actual_array .equalsExpected. expected_array)
      test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. (cols-1)*rows)
      test_diagnosis = test_diagnosis .also. ((new_line('') .occurrencesIn. stop_code) .equalsExpected. rows-1)
    end associate
  end function

end module character_stop_code_test_m
