! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_vector_test_description_m
  !! Define an abstraction for describing test intentions and array-valued test functions
  use julienne_string_m, only : string_t
  use julienne_test_result_m, only : test_result_t
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  private
  public :: vector_test_description_t
  public :: vector_diagnosis_function_i
#ifdef __GFORTRAN__
  public :: run
#endif

  abstract interface
    function vector_diagnosis_function_i() result(diagnoses)
      import test_diagnosis_t
      implicit none
      type(test_diagnosis_t), allocatable :: diagnoses(:)
    end function
  end interface

  type vector_test_description_t
    private
    type(string_t), allocatable :: descriptions_(:)
    procedure(vector_diagnosis_function_i), pointer, nopass :: vector_diagnosis_function_
  contains
    procedure run
    procedure contains_text
  end type

  interface vector_test_description_t

    module function construct_from_strings(descriptions, vector_diagnosis_function) result(vector_test_description)
     !! The result is a vector_test_description_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: descriptions(:)
      procedure(vector_diagnosis_function_i), intent(in), pointer :: vector_diagnosis_function
      type(vector_test_description_t) vector_test_description
    end function

  end interface

  interface

    impure module function run(self) result(test_results)
      !! The result encapsulates the test description and test outcome
      implicit none
      class(vector_test_description_t), intent(in) :: self
      type(test_result_t), allocatable :: test_results(:)
    end function

    module function contains_text(self, substring) result(match_vector)
      !! The result is .true. if the test description includes the value of substring 
      implicit none
      class(vector_test_description_t), intent(in) :: self
      character(len=*), intent(in) :: substring
      logical, allocatable :: match_vector(:)
    end function

  end interface

end module julienne_vector_test_description_m
