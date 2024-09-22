! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module command_line_test_m
  !! Verify object pattern asbtract parent
  use julienne_m, only : test_t, test_result_t, command_line_t, test_description_substring, string_t, test_description_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
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
      test_description_t(string_t("returning the value passed after a command-line flag"), check_flag_value), &
      test_description_t(string_t("returning an empty string when a flag value is missing"), handle_missing_flag_value), &
      test_description_t(string_t("detecting a present command-line argument"), check_command_line_argument) &
    ]   
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_flag_ptr, handle_missing_value_ptr, check_command_ptr
    check_flag_ptr => check_flag_value 
    handle_missing_value_ptr => handle_missing_flag_value
    check_command_ptr => check_command_line_argument
    test_descriptions = [ & 
      test_description_t(string_t("returning the value passed after a command-line flag"), check_flag_ptr), &
      test_description_t(string_t("returning an empty string when a flag value is missing"), handle_missing_value_ptr), &
      test_description_t(string_t("detecting a present command-line argument"), check_command_ptr) &
    ]   
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function check_flag_value() result(test_passes)
    logical test_passes
    integer exit_status, command_status
    character(len=132) command_message

    call execute_command_line( &
      command = "fpm run --example get-flag-value -- --input-file some_file_name", &
      wait = .true., exitstat = exit_status, cmdstat = command_status, cmdmsg = command_message &
    )   
    test_passes = exit_status == 0 
  end function

  function handle_missing_flag_value() result(test_passes)
    logical test_passes
    integer exit_status, command_status
    character(len=132) command_message

    call execute_command_line( &
      command = "fpm run --example handle-missing-flag -- --empty-flag", &
      wait = .true., exitstat = exit_status, cmdstat = command_status, cmdmsg = command_message &
    )   
    test_passes = exit_status == 0 
  end function

  function check_command_line_argument() result(test_passes)
    logical test_passes
    integer exit_status, command_status
    character(len=132) command_message

    call execute_command_line( &
      command = "fpm run --example check-command-line-argument -- --some-argument", &
      wait = .true., exitstat = exit_status, cmdstat = command_status, cmdmsg = command_message &
    )
    test_passes = exit_status == 0
  end function

end module command_line_test_m
