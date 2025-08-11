! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module command_line_test_m
  !! Verify object pattern asbtract parent
  use julienne_m, only : &
     command_line_t &
    ,GitHub_CI &
    ,operator(.equalsExpected.) &
    ,operator(.expect.) &
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
    type(command_line_t) command_line

    skip_all_tests_if_running_github_ci: &
    if (GitHub_CI()) then
      test_descriptions = [ &
         test_description_t(string_t("flag_value() result is the value passed after a command-line flag")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing")) &
        ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing")) &
        ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present")) &
      ]
      print "(*(a))"  &
        ,new_line('') &
        ,"----> Skipping the command_line_t tests in GitHub CI.", new_line('') &
        ,"----> To test locally, append the following flags to the 'fpm test' command: -- --test command_line_t --type" &
        ,new_line('')
    else if (.not. command_line%argument_present(["--test"])) then ! skip the tests if not explicitly requested
      test_descriptions = [ &
         test_description_t(string_t("flag_value() result is the value passed after a command-line flag")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing")) &
        ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing")) &
        ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present")) &
      ]
      print "(*(a))"  &
        ,new_line('') &
        ,"-----> To test command_line_t, append the following to the 'fpm test' command: -- --test command_line_t --type" &
        ,new_line('')
    else ! run the tests
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
      test_descriptions = [ &
         test_description_t(string_t("flag_value() result is the value passed after a command-line flag"), check_flag_value) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing"), check_flag_value_missing) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing"), check_flag_missing) &
        ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing"), check_argument_missing) &
        ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present"), check_argument_present) &
      ]
#else
      gfortran_work_around: &
      block
        procedure(diagnosis_function_i), pointer :: &
           check_flag_value_ptr         => check_flag_value &
          ,check_flag_value_missing_ptr => check_flag_value_missing &
          ,check_flag_missing_ptr       => check_flag_missing &
          ,check_argument_missing_ptr   => check_argument_missing &
          ,check_argument_present_ptr   => check_argument_present

        test_descriptions = [ &
           test_description_t(string_t("flag_value() result is the value passed after a command-line flag"), check_flag_value_ptr) &
          ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing"), check_flag_value_missing_ptr) &
          ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing"), check_flag_missing_ptr) &
          ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing"), check_argument_missing_ptr) &
          ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present"), check_argument_present_ptr) &
        ]
      end block gfortran_work_around
#endif
    end if skip_all_tests_if_running_github_ci

    test_results = test_descriptions%run()
  end function

  function check_flag_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    test_diagnosis = command_line%flag_value("--test") .equalsExpected. "command_line_t"
  end function

  function check_flag_value_missing() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    test_diagnosis = command_line%flag_value("--type") .equalsExpected. ""
  end function

  function check_flag_missing() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    test_diagnosis = command_line%flag_value("r@nd0m.Junk-H3R3") .equalsExpected. ""
  end function

  function check_argument_missing() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    test_diagnosis = .expect. (.not. command_line%argument_present(["M1ss1ng-argUment"]))
  end function

  function check_argument_present() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(command_line_t) command_line
    test_diagnosis = .expect. command_line%argument_present(["--type"])
  end function

end module command_line_test_m
