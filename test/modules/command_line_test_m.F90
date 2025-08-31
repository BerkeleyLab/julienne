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
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,bless &
    ,test_t

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
    type(command_line_test_t) command_line_test
    type(command_line_t) command_line


#if HAVE_MULTI_IMAGE_SUPPORT
    image_number: &
    associate(me => this_image())
#else
    image_number: &
    associate(me => 1)
#endif

    skip_all_tests_if_running_github_ci: &
    if (GitHub_CI()) then
      test_descriptions = [ &
         test_description_t(string_t("flag_value() result is the value passed after a command-line flag")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing")) &
        ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing")) &
        ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present")) &
      ]
      if (me==1) then
        print '(a)', &
          new_line('') &
          // "----> Skipping the command_line_t tests in GitHub CI." // new_line('') &
          // "----> To test locally, append the following flags to the 'fpm test' command: -- --test command_line_t --type" &
          // new_line('')
      end if
    else if (.not. command_line%argument_present(["--test"])) then ! skip the tests if not explicitly requested
      test_descriptions = [ &
         test_description_t(string_t("flag_value() result is the value passed after a command-line flag")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing")) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing")) &
        ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing")) &
        ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present")) &
      ]
      if (me==1) then
        print '(a)', &
          new_line('') &
          // "-----> To test command_line_t, append the following to the 'fpm test' command: -- --test command_line_t --type" &
          // new_line('')
      end if
    else ! run the tests
      test_descriptions = [ &
         test_description_t(string_t("flag_value() result is the value passed after a command-line flag"), bless(check_flag_value)) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag value is missing"), bless(check_flag_value_missing)) &
        ,test_description_t(string_t("flag_value() result is an empty string if command-line flag is missing"), bless(check_flag_missing)) &
        ,test_description_t(string_t("argument_present() result is .false. if a command-line argument is missing"), bless(check_argument_missing)) &
        ,test_description_t(string_t("argument_present() result is .true. if a command-line argument is present"), bless(check_argument_present)) &
      ]
    end if skip_all_tests_if_running_github_ci

    end associate image_number

    test_results = command_line_test%run(test_descriptions)
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
