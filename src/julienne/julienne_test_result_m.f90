! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_test_result_m
  !! Define an abstraction for describing test results the test description and,
  !! if the test was not skipped, then also a test diagnosis.
  use julienne_string_m, only : string_t
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  private
  public :: test_result_t

  type test_result_t
    !! Encapsulate a test-description string and optionally a test diagnosis.
    !! This type is similar to test_description_t and test_diagnosis_t type but
    !! 1. Doesn't need the former's procedure(diagnosis_function_i) component and
    !! 2. Allocates an instance of the latter if and only if the test wasn't skipped.
    private
    type(string_t) :: description_
    type(test_diagnosis_t), allocatable :: diagnosis_
  contains
    procedure :: characterize
    procedure :: passed
    procedure :: skipped
    generic :: description_contains => description_contains_string, description_contains_characters
    procedure, private ::              description_contains_string, description_contains_characters
  end type

  interface test_result_t

    elemental module function construct_from_string(description, diagnosis) result(test_result)
      !! The result is a test_result_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: description
      type(test_diagnosis_t), intent(in), optional :: diagnosis
      type(test_result_t) test_result 
    end function

    elemental module function construct_from_character(description, diagnosis) result(test_result)
      !! The result is a test_result_t object with the components defined by the dummy arguments
      implicit none
      character(len=*), intent(in) :: description
      type(test_diagnosis_t), intent(in), optional :: diagnosis
      type(test_result_t) test_result 
    end function

  end interface

  interface

    pure module function characterize(self) result(characterization)
      !! The result is a character description of the test and its outcome
      implicit none
      class(test_result_t), intent(in) :: self
      character(len=:), allocatable :: characterization
    end function

    impure elemental module function passed(self) result(test_passed)
      !! The result is true if and only if the test passed on all images
      implicit none
      class(test_result_t), intent(in) :: self
      logical test_passed
    end function

    impure elemental module function skipped(self) result(test_skipped)
      !! The result is true if and only if the test result contains no diagnosis on any image
      implicit none
      class(test_result_t), intent(in) :: self
      logical test_skipped
    end function

    elemental module function description_contains_string(self, substring) result(substring_found)
      !! The result is true if and only if the test description contains the substring
      implicit none
      class(test_result_t), intent(in) :: self
      type(string_t), intent(in) :: substring
      logical substring_found
    end function

    elemental module function description_contains_characters(self, substring) result(substring_found)
      !! The result is true if and only if the test description contains the substring
      implicit none
      class(test_result_t), intent(in) :: self
      character(len=*), intent(in) :: substring
      logical substring_found
    end function

  end interface

end module julienne_test_result_m
