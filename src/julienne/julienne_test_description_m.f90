! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_test_description_m
  !! Define an abstraction for describing test intentions and test functions
  use julienne_string_m, only : string_t
  use julienne_test_result_m, only : test_result_t
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  private
  public :: test_description_t
  public :: diagnosis_function_i

  abstract interface

    function diagnosis_function_i() result(test_diagnosis)
      import test_diagnosis_t
      implicit none
      type(test_diagnosis_t) test_diagnosis
    end function

  end interface

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

  end interface

end module julienne_test_description_m
