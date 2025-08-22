! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_test_description_m
  !! Define an abstraction for describing test intentions and test functions
  use julienne_string_m, only : string_t
  use julienne_test_result_m, only : test_result_t
  use julienne_test_diagnosis_m, only : test_diagnosis_t, diagnosis_function_i
  implicit none

  private
  public :: test_description_t
  public :: filter

  type test_description_t
    !! Encapsulate test descriptions and test-functions
    private
    character(len=:), allocatable :: description_
    procedure(diagnosis_function_i), pointer, nopass :: diagnosis_function_ => null()
  contains
    procedure run
    generic :: contains_text => contains_string_t, contains_characters
    procedure, private ::       contains_string_t, contains_characters
    generic :: operator(==) => equals
    procedure, private ::      equals
  end type

  interface test_description_t

    module function construct_from_string(description, diagnosis_function) result(test_description)
      !! The result is a test_description_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: description
      procedure(diagnosis_function_i), intent(in), pointer, optional :: diagnosis_function
      type(test_description_t) test_description
    end function

    module function construct_from_characters(description, diagnosis_function) result(test_description)
      !! The result is a test_description_t object with the components defined by the dummy arguments
      implicit none
      character(len=*), intent(in) :: description
      procedure(diagnosis_function_i), intent(in), pointer, optional :: diagnosis_function
      type(test_description_t) test_description
    end function

  end interface

  interface

    impure elemental module function run(self) result(test_result)
      !! The result encapsulates the test description and test outcome
      implicit none
      class(test_description_t), intent(in) :: self
      type(test_result_t) test_result
    end function

    elemental module function contains_string_t(self, substring) result(match)
      !! The result is .true. if the test description includes the value of substring
      implicit none
      class(test_description_t), intent(in) :: self
      type(string_t), intent(in) :: substring
      logical match
    end function

    elemental module function contains_characters(self, substring) result(match)
      !! The result is .true. if the test description includes the value of substring
      implicit none
      class(test_description_t), intent(in) :: self
      character(len=*), intent(in) :: substring
      logical match
    end function

    elemental module function equals(lhs, rhs) result(lhs_eq_rhs)
      !! The result is .true. if the components of the lhs & rhs are equal
      implicit none
      class(test_description_t), intent(in) :: lhs, rhs
      logical lhs_eq_rhs
    end function

    module function filter(test_descriptions, subject) result(filtered_test_descriptions)
      !! The result is .true. an array of test_description_t objects whose description_ or contains the substring specified 
      !! by command-line --contains flag if present, or all test_descriptions if the subject contains the same substring 
      implicit none
      type(test_description_t), intent(in) :: test_descriptions(:)
      character(len=*), intent(in) :: subject
      type(test_description_t), allocatable :: filtered_test_descriptions(:)
    end function

  end interface

end module julienne_test_description_m
