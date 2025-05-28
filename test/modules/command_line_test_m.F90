! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module command_line_test_m
  !! Verify object pattern asbtract parent
  use julienne_m, only : &
     command_line_t &
    ,string_t &
    ,test_description_substring &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif

  implicit none

  private
  public :: command_line_test_t

  type, extends(test_t) :: command_line_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The command_line_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
       test_description_t(string_t("flag_value() result is the value passed after a command-line flag"), check_flag_value) &
      ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing"), check_flag_value_missing) &
      ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing"), check_flag_missing) &
      ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing"), check_argument_missing) &
      ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present"), check_argument_present) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: &
       check_flag_value_ptr &
      ,check_flag_value_missing_ptr &
      ,check_flag_missing_ptr &
      ,check_argument_missing_ptr &
      ,check_argument_present_ptr

      check_flag_value_ptr         => check_flag_value
      check_flag_value_missing_ptr => check_flag_value_missing
      check_flag_missing_ptr       => check_flag_missing
      check_argument_missing_ptr   => check_argument_missing
      check_argument_present_ptr   => check_argument_present

    test_descriptions = [ &
       test_description_t(string_t("flag_value() result is the value passed after a command-line flag"), check_flag_value_ptr) &
      ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing"), check_flag_value_missing_ptr) &
      ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing"), check_flag_missing_ptr) &
      ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing"), check_argument_missing_ptr) &
      ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present"), check_argument_present_ptr) &
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function check_flag_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    character(len=*), parameter :: expected_flag_value = "command_line_t"

    associate(actual_flag_value => command_line%flag_value("--test"))
      test_diagnosis = test_diagnosis_t( &
         test_passed = expected_flag_value == actual_flag_value &
        ,diagnostics_string = "expected " // expected_flag_value // ", actual "  // actual_flag_value &
      )
    end associate
  end function

  function check_flag_value_missing() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    character(len=*), parameter :: expected_flag_value = ""

    associate(actual_flag_value => command_line%flag_value("--type"))
      test_diagnosis = test_diagnosis_t( &
         test_passed = expected_flag_value == actual_flag_value &
        ,diagnostics_string = "expected '" // expected_flag_value // "', actual '"  // actual_flag_value // "'" &
      )
    end associate
  end function

  function check_flag_missing() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    character(len=*), parameter :: expected_flag_value = ""

    associate(actual_flag_value => command_line%flag_value("r@nd0m.Junk-H3R3"))
      test_diagnosis = test_diagnosis_t( &
         test_passed = expected_flag_value == actual_flag_value &
        ,diagnostics_string = "expected '" // expected_flag_value // "', actual '"  // actual_flag_value // "'" &
      )
    end associate
  end function

  function check_argument_missing() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    character(len=*), parameter :: expected_flag_value = ""

    associate(argument_found => command_line%argument_present(["M1ss1ng-argUment"]))
      test_diagnosis = test_diagnosis_t( &
         test_passed = .not. argument_found &
        ,diagnostics_string = "expected .false., actual .true." &
      )
    end associate
  end function

  function check_argument_present() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line

    associate(argument_found => command_line%argument_present(["--type"]))
      test_diagnosis = test_diagnosis_t( &
         test_passed = argument_found &
        ,diagnostics_string = "expected .true., actual .false." &
      )
    end associate
  end function

end module command_line_test_m
