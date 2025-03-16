! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_description_test_m
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
  public :: test_description_test_t

  type, extends(test_t) :: test_description_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_description_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:), vector_test_results(:)
    type(test_description_t), allocatable :: scalar_test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    scalar_test_descriptions = [ &
      test_description_t("identical construction from string_t or character arguments", check_character_constructor) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: check_character_ptr
    check_character_ptr => check_character_constructor
    scalar_test_descriptions = [ &
      test_description_t("identical construction from string_t or character arguments", check_character_ptr) &
    ]
#endif

    associate(substring_in_subject => index(subject(), test_description_substring) /= 0)

      associate(substring_in_scalar_test_description => scalar_test_descriptions%contains_text(test_description_substring))
        scalar_test_descriptions = pack(scalar_test_descriptions, substring_in_subject .or. substring_in_scalar_test_description)
      end associate

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
              vector_test_results = [(vector_test_descriptions(i)%run(), i=1,num_vector_tests)]
            else
              associate(substring_in_description_vector => &
                [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)] &
              )
                associate(matching_vector_tests => pack(vector_test_descriptions, substring_in_description_vector))
                  associate(results_with_matches => [(matching_vector_tests(i)%run(), i=1,size(matching_vector_tests))])
                    vector_test_results = pack(results_with_matches, results_with_matches%description_contains(test_description_substring))
                  end associate
                end associate
              end associate
            end if
            test_results = [scalar_test_descriptions%run(), vector_test_results]
          end block
        end associate
      end associate
    end associate

  end function

  function check_character_constructor() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_diagnosis = test_diagnosis_t( &
       test_passed = test_description_t("foo", tautology) == test_description_t(string_t("foo"), tautology) &
      ,diagnostics_string = 'test_description_t("foo", tautology) /= test_description_t(string_t("foo"), tautology)' &
    )
#else
    procedure(diagnosis_function_i), pointer :: tautology_ptr
    tautology_ptr => tautology

    test_diagnosis = test_diagnosis_t(  &
       test_passed = test_description_t("foo", tautology_ptr) == test_description_t(string_t("foo"), tautology_ptr) &
      ,diagnostics_string= 'test_description_t("foo", tautology_ptr) /= test_description_t(string_t("foo"), tautology__ptr)'&
    )
#endif
  contains
    type(test_diagnosis_t) function tautology()
      tautology = test_diagnosis_t(.true.,"")
    end function
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

end module test_description_test_m
