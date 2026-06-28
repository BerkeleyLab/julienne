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
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
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
       test_description_t(string_t("converting a 1D arrays to a comma-separated-value (CSV) strings"), usher(check_1D_array)) &
      ,test_description_t(string_t("converting a 2D arrays to new-line-separated CSV strings"), usher(check_2D_array)) &
      ,test_description_t(string_t("converting a 3D arrays to new-line-separated CSV strings"), usher(check_3D_array)) &
    ]
    test_results = character_stop_code_test%run(test_descriptions)
  end function

  pure function occurrences_in(lhs, rhs) result(occurrences)
    character(len=1), intent(in) :: lhs
    character(len=*), intent(in) :: rhs
    integer occurrences, c
    occurrences = count([(rhs(c:c)==lhs, c=1,len(rhs))])
  end function

  function search_and_replace(string, search_for, replace_with, except_final) result(replacement_string)
    character(len=*), intent(in) :: string
    character(len=1), intent(in) :: search_for, replace_with
    character(len=:), allocatable :: replacement_string
    logical, intent(in), optional :: except_final
    integer c, c_final, c_

    allocate(character(len=len(string)) :: replacement_string)

    c_final = 0
    c_ = 0
    do c = 1, len(string)
      c_ = c_ + 1
      if (string(c:c)==search_for) then
        c_final = c
        if (c>1) then
          if (string(c-1:c-1) /= search_for) then
            replacement_string(c_:c_) = replace_with
          else
            c_ = c_ - 1
          end if
        end if
      else
        replacement_string(c_:c_) = string(c:c)
      end if
    end do
    if (present(except_final)) then
      if (except_final .and. c_final>0) replacement_string(c_final:c_final) = search_for
    end if

    replacement_string = trim(replacement_string)
  end function

  function check_1D_array() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    integer, parameter :: expected_array(*) = [1,2,3,4]
    integer actual_array(size(expected_array))

    real, parameter :: expected_real_array(*) = real(expected_array)
    real actual_real_array(size(expected_real_array,1))

    complex, parameter :: i = (0.,1.)
    complex, parameter :: expected_complex_array(*) = cmplx(expected_array) - expected_array*i
    complex actual_complex_array(size(expected_complex_array,1))

    double precision, parameter :: expected_dble_array(*) = dble(expected_array)
    double precision actual_dble_array(size(expected_dble_array,1))

    test_diagnosis = passing_test()

    associate(stop_code => character_stop_code(expected_array))
      read(stop_code,*) actual_array
      test_diagnosis = test_diagnosis .also. .all. (actual_array .equalsExpected. expected_array)
      test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. size(expected_array)-1)  &
        // " commas in " // stop_code
    end associate

    associate(stop_code => character_stop_code(expected_real_array))
      read(stop_code,*) actual_real_array
      test_diagnosis = test_diagnosis .also. .all. (actual_real_array .approximates. real(expected_array) .within. 0.)
      test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. size(expected_real_array)-1) &
        // " commas in " // stop_code
    end associate

    associate(stop_code => character_stop_code(expected_complex_array), expected_imaginary_part => -expected_array*i)
      read(stop_code,*) actual_complex_array
      test_diagnosis = test_diagnosis .also. .all. (actual_complex_array%re .approximates. real(expected_array) .within. 0.)
      test_diagnosis = test_diagnosis .also. .all. (actual_complex_array%im .approximates. expected_imaginary_part %im .within. 0.)
    end associate

    associate(stop_code => character_stop_code(expected_dble_array))
      read(stop_code,*) actual_dble_array
      test_diagnosis = test_diagnosis .also. .all. (actual_dble_array .approximates. dble(expected_array) .within. 0D0)
      test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. size(expected_dble_array)-1) &
        // " commas in " // stop_code
    end associate
  end function

  function check_2D_array() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    integer, parameter :: expected_array(*,*) = reshape([11,21,12,22,13,23], [2,3])
    integer actual_array(size(expected_array,1),size(expected_array,2))

    real, parameter :: expected_real_array(*,*) = real(expected_array)
    real  actual_real_array(size(expected_array,1),size(expected_array,2))

    double precision, parameter :: expected_dble_array(*,*) = dble(expected_array)
    double precision actual_dble_array(size(expected_dble_array,1), size(expected_dble_array,2))

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

    associate( &
       stop_code => character_stop_code(expected_real_array) &
      ,rows => size(expected_real_array,1) &
      ,cols => size(expected_real_array,2) &
    )
      associate(one_line => search_and_replace(stop_code, search_for=new_line(''), replace_with=","))
        read(one_line,*) actual_real_array(1,:), actual_real_array(2,:)
        test_diagnosis = test_diagnosis .also. .all. (actual_real_array .approximates. real(expected_array) .within. 0.)
        test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. (cols-1)*rows)
        test_diagnosis = test_diagnosis .also. ((new_line('') .occurrencesIn. stop_code) .equalsExpected. rows-1)
      end associate
    end associate

    associate( &
       stop_code => character_stop_code(expected_dble_array) &
      ,rows => size(expected_dble_array,1) &
      ,cols => size(expected_dble_array,2) &
    )
      associate(one_line => search_and_replace(stop_code, search_for=new_line(''), replace_with=","))
        read(one_line,*) actual_dble_array(1,:), actual_dble_array(2,:)
        test_diagnosis = test_diagnosis .also. .all. (actual_dble_array .approximates. dble(expected_array) .within. 0D0)
        test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. (cols-1)*rows)
        test_diagnosis = test_diagnosis .also. ((new_line('') .occurrencesIn. stop_code) .equalsExpected. rows-1)
      end associate
    end associate
  end function

  function check_3D_array() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_array(*,*,*) = reshape([111,211,121,221, 112,212,122,222, 113,213,123,223], [2,2,3])
    integer actual_array(size(expected_array,1),size(expected_array,2),size(expected_array,3))

    real, parameter :: expected_real_array(*,*,*) = real(expected_array)
    real  actual_real_array(size(expected_real_array,1),size(expected_real_array,2),size(expected_real_array,3))

    double precision, parameter :: expected_dble_array(*,*,*) = dble(expected_array)
    double precision actual_dble_array(size(expected_dble_array,1), size(expected_dble_array,2), size(expected_dble_array,3))

    test_diagnosis = passing_test()

    associate( &
       stop_code => character_stop_code(expected_array) &
      ,rows  => size(expected_array,1) &
      ,cols  => size(expected_array,2) &
      ,pages => size(expected_array,3) &
    )
      associate(one_line => search_and_replace(stop_code, search_for=new_line(''), replace_with=","))
        read(one_line,'(*(i3,1x))') actual_array(1,:,1), actual_array(2,:,1), actual_array(1,:,2), actual_array(2,:,2), actual_array(1,:,3), actual_array(2,:,3)
        test_diagnosis = test_diagnosis .also. .all. (actual_array .equalsExpected. expected_array)
        test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. (cols-1)*rows*pages) // " commas"
        test_diagnosis = test_diagnosis .also. ((new_line('') .occurrencesIn. stop_code) .equalsExpected. (rows*pages-1) + (pages-1)) &
          // " new-line characters"
      end associate
    end associate

    associate( &
       stop_code => character_stop_code(expected_real_array) &
      ,rows  => size(expected_real_array,1) &
      ,cols  => size(expected_real_array,2) &
      ,pages => size(expected_real_array,3) &
    )
      associate(one_line => trim(search_and_replace(stop_code, search_for=new_line(''), replace_with=",")))
        read(one_line(1:179),*) actual_real_array(1,:,1), actual_real_array(2,:,1), actual_real_array(1,:,2), actual_real_array(2,:,2), actual_real_array(1,:,3), actual_real_array(2,:,3)
        test_diagnosis = test_diagnosis .also. .all. (actual_real_array .approximates. expected_real_array .within. 0.)
        test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. (cols-1)*rows*pages) // " commas"
        test_diagnosis = test_diagnosis .also. ((new_line('') .occurrencesIn. stop_code) .equalsExpected. (rows*pages-1) + (pages-1)) &
          // " new-line characters"
      end associate
    end associate

    associate( &
       stop_code => character_stop_code(expected_dble_array) &
      ,rows  => size(expected_dble_array,1) &
      ,cols  => size(expected_dble_array,2) &
      ,pages => size(expected_dble_array,3) &
    )
      associate(one_line => trim(search_and_replace(stop_code, search_for=new_line(''), replace_with=",")))
        read(one_line(1:179),*) actual_dble_array(1,:,1), actual_dble_array(2,:,1), actual_dble_array(1,:,2), actual_dble_array(2,:,2), actual_dble_array(1,:,3), actual_dble_array(2,:,3)
        test_diagnosis = test_diagnosis .also. .all. (actual_dble_array .approximates. expected_dble_array .within. 0D0)
        test_diagnosis = test_diagnosis .also. (("," .occurrencesIn. stop_code) .equalsExpected. (cols-1)*rows*pages) // " commas"
        test_diagnosis = test_diagnosis .also. ((new_line('') .occurrencesIn. stop_code) .equalsExpected. (rows*pages-1) + (pages-1)) &
          // " new-line characters"
      end associate
    end associate
  end function

end module character_stop_code_test_m
