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
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    use julienne_vector_test_description_m, only : vector_diagnosis_function_i
#endif
#ifdef __GFORTRAN__
    use julienne_vector_test_description_m, only : run
#endif
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
#ifndef __GFORTRAN__
      associate(vector_test_descriptions => [ &
        vector_test_description_t( [ &
           string_t(    "finding a substring in a test description") &
          ,string_t("not finding a missing substring in a test description") &
        ], check_substring_search &
      )]) 
#else
      block
       type(vector_test_description_t) vector_test_descriptions(1)

       vector_test_descriptions(1) = &
         vector_test_description_t( [ &
            string_t(    "finding a substring in a test description") &
           ,string_t("not finding a missing substring in a test description") &
       ])
#endif
        associate(num_vector_tests => size(vector_test_descriptions))
          block
            integer i

            if (substring_in_subject) then
              test_results = [(vector_test_descriptions(i)%run(), i=1,num_vector_tests)]
            else
#ifndef __GFORTRAN__
              associate(substring_in_description_vector => &
                [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)] &
              )
#else
              block
                logical, allocatable :: substring_in_description_vector(:)
                substring_in_description_vector = &
                  [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)]
#endif
#ifndef __GFORTRAN__
                associate(matching_vector_tests => pack(vector_test_descriptions, substring_in_description_vector))
                  associate(results_with_matches => [(matching_vector_tests(i)%run(), i=1,size(matching_vector_tests))])
                    test_results = pack(results_with_matches, results_with_matches%description_contains(test_description_substring))
                  end associate
                end associate
#else
                  block
                    type(test_result_t), allocatable :: results_with_matches(:)
                    type(vector_test_description_t), allocatable :: matching_vector_tests(:)
                    matching_vector_tests = pack(vector_test_descriptions, substring_in_description_vector)
                    results_with_matches = [(run(matching_vector_tests(i)), i=1,size(matching_vector_tests))]
                    test_results = pack(results_with_matches, results_with_matches%description_contains(test_description_substring))
                  end block
#endif
#ifndef __GFORTRAN__
              end associate
#else
              end block
#endif
            end if
          end block
        end associate
#ifndef __GFORTRAN__
      end associate
#else
      end block
#endif
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
