! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module vector_test_description_test_m
  !! Verify vector_test_description_t object behavior
  use julienne_m, only : &
    string_t, test_result_t, vector_test_description_t, test_t, test_description_substring, vector_function_strategy_t
  implicit none

  private
  public :: vector_test_description_test_t

  type, extends(test_t) :: vector_test_description_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  type, extends(vector_function_strategy_t) :: two_vector_tautology_t
  contains
    procedure, nopass :: vector_function
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The vector_test_description_t type" 
  end function

  function vector_function() result(passed)
    logical, allocatable :: passed(:)
    passed = [.true., .true.]
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(two_vector_tautology_t) two_vector_tautology

    associate( &
      vector_test_description => vector_test_description_t([string_t("construction"),string_t("assignment")], two_vector_tautology)&
    )
      associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
        associate(substring_in_description => vector_test_description%contains_text(test_description_substring))
        if (substring_in_subject) then
          test_results = vector_test_description%run()
        else if (any(substring_in_description)) then
          test_results = vector_test_description%run()
          test_results = pack(test_results, test_results%description_contains(string_t(test_description_substring)))
         else
          test_results = [test_result_t::]
        end if
        end associate
      end associate
    end associate
  end function

end module vector_test_description_test_m
