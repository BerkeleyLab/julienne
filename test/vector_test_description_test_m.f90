! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module vector_test_description_test_m
  !! Verify test_description_t object behavior
  use julienne_m, only : &
     diagnosis_function_i &
    ,string_t &
    ,test_result_t &
    ,test_description_t &
    ,test_description_substring &
    ,test_diagnosis_t &
    ,test_t &
    ,vector_test_description_t
  implicit none

  private
  public :: vector_test_description_test_t

  type, extends(test_t) :: vector_test_description_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The vector_test_description_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:), vector_test_results(:)

    associate(substring_in_subject => index(subject(), test_description_substring) /= 0)
      associate(vector_test_descriptions => [ &
        vector_test_description_t( [ &
           string_t(    "finding a substring in a test description") &
          ,string_t("not finding a missing substring in a test description") &
        ], check_substring_search &
      )]) 
        associate(num_vector_tests => size(vector_test_descriptions))
          block
            integer i
           
            if (substring_in_subject) then
              test_results = [(vector_test_descriptions(i)%run(), i=1,num_vector_tests)]
            else
              associate(substring_in_description_vector => &
                [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)] &
              )
                associate(matching_vector_tests => pack(vector_test_descriptions, substring_in_description_vector))
                  associate(results_with_matches => [(matching_vector_tests(i)%run(), i=1,size(matching_vector_tests))])
                    test_results = pack(results_with_matches, results_with_matches%description_contains(test_description_substring))
                  end associate
                end associate
              end associate
            end if
          end block
        end associate
      end associate
    end associate
  end function

  function check_substring_search() result(diagnoses)
    type(test_diagnosis_t), allocatable :: diagnoses(:)
    procedure(diagnosis_function_i), pointer :: unused

    unused => null()

    associate(doing_something => test_description_t("doing something", unused))
      diagnoses = [ &
         test_diagnosis_t(test_passed =       doing_something%contains_text("something"),    diagnostics_string="expected .true.") &
        ,test_diagnosis_t(test_passed = .not. doing_something%contains_text("missing text"), diagnostics_string="expected .true.") &
      ]
    end associate
  end function

end module vector_test_description_test_m
