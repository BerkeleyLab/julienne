! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_test_description_m
  !! Define an abstraction for describing test intentions and test functions
  use julienne_string_m, only : string_t
  use julienne_test_result_m, only : test_result_t
  implicit none

  private
  public :: test_description_t
  public :: test_function_i

  abstract interface
    function test_function_i() result(passes)
      implicit none
      logical passes
    end function
  end interface

  type test_description_t
    !! Encapsulate test descriptions and test-functions
    private
    type(string_t) description_
    procedure(test_function_i), pointer, nopass :: test_function_ => null()
  contains
    procedure run
    procedure contains_text
    generic :: operator(==) => equals
    procedure, private :: equals
  end type

  interface test_description_t

    module function construct_from_string_t(description, test_function) result(test_description)
      !! The result is a test_description_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: description
      procedure(test_function_i), intent(in), pointer :: test_function
      type(test_description_t) test_description
    end function

    module function construct_from_character(description, test_function) result(test_description)
      !! The result is a test_description_t object with the components defined by the dummy arguments
      implicit none
      character(len=*), intent(in) :: description
      procedure(test_function_i), intent(in), pointer :: test_function
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

    impure elemental module function contains_text(self, substring) result(match)
      !! The result is .true. if the test description includes the value of substring 
      implicit none
      class(test_description_t), intent(in) :: self
      type(string_t), intent(in) :: substring
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
