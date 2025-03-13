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
    ,test_diagnosis_t&
    ,test_t &
    ,vector_function_strategy_t &
    ,vector_test_description_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: test_description_test_t

  type, extends(test_t) :: test_description_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  type, extends(vector_function_strategy_t) :: substring_search_test_function_t
  contains
    procedure, nopass :: vector_function => check_substring_search
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_description_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:), vector_test_results(:)
    type(test_description_t), allocatable :: scalar_test_descriptions(:)
    type(vector_test_description_t), allocatable :: vector_test_descriptions(:)
    type(substring_search_test_function_t) substring_search_test_function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    scalar_test_descriptions = [ &
      test_description_t("identical construction from string_t or character arguments", check_character_constructor) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_character_ptr
    check_character_ptr => check_character_constructor
    scalar_test_descriptions = [ &
      test_description_t("identical construction from string_t or character arguments", check_character_ptr) &
    ]
#endif

    vector_test_descriptions = [ &
      vector_test_description_t( &
        [  string_t("finding a string_t substring in a test description") &
          ,string_t("finding an assumed-length character substring in a test description") &
          ,string_t("not finding a missing string_t substring in a test description") &
          ,string_t("not finding a missing assumed-length character substring in a test description") &
        ], substring_search_test_function &
      ) &
    ]

    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => scalar_test_descriptions%contains_text(string_t(test_description_substring)), &
      num_vector_tests => size(vector_test_descriptions) &
    )
      scalar_test_descriptions = pack(scalar_test_descriptions, substring_in_subject .or. substring_in_description)

      block
        integer i

        associate( &
          substring_in_description_vector => &
            [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)] &
        )
          if (substring_in_subject) then
            vector_test_results = [(vector_test_descriptions(i)%run(), i=1,num_vector_tests)]
          else if (any(substring_in_description_vector)) then
              vector_test_descriptions = pack(vector_test_descriptions, substring_in_description_vector)
              vector_test_results =  [(vector_test_descriptions(i)%run(), i=1,size(vector_test_descriptions))]
              vector_test_results =  &
                pack(vector_test_results, vector_test_results%description_contains(string_t(test_description_substring)))
           else
            vector_test_results = [test_result_t::]
          end if
          test_results = [scalar_test_descriptions%run(), vector_test_results]
        end associate
      end block
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
       test_passed = test_description_t("foo", test_function_ptr) == test_description_t(string_t("foo"), test_function_ptr) &
      ,diagnostics_string= 'test_description_t("foo", tautology_ptr) /= test_description_t(string_t("foo"), tautology__ptr)'&
    )
#endif
  contains
    logical function tautology()
      tautology = .true. 
    end function
  end function

  function check_substring_search() result(tests_pass)
    logical, allocatable :: tests_pass(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    procedure(diagnosis_function_i), pointer :: unused
    unused => null()

    test_descriptions = [ &
       test_description_t("an example substring"     , unused) &
      ,test_description_t("another example substring", unused) &
      ,test_description_t("moving right along"       , unused) &
      ,test_description_t("nothing to see here"      , unused) &
    ]
    tests_pass = [ &
             test_descriptions(1)%contains_text(string_t("example")       ), &
             test_descriptions(2)%contains_text(         "example"        ), &
       .not. test_descriptions(3)%contains_text(string_t("missing string")), &
       .not. test_descriptions(4)%contains_text(         "missing string" ) &
    ]
  end function

end module test_description_test_m
