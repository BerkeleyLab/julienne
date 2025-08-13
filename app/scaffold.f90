! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program scaffold
  use julienne_m, only : command_line_t, file_t, test_suite_t
  implicit none
  type(command_line_t) command_line
  type(file_t) driver
  character(len=:), allocatable ::  path, subjects_file_name

  if (help_requested()) call print_usage_info_and_stop

  subjects_file_name = command_line%flag_value("--subjects")
  if (len(subjects_file_name) == 0) call print_usage_info_and_stop

  print '(*(a))', "Reading test subject information from " // subjects_file_name

  associate(test_suite => test_suite_t(file_t(subjects_file_name)))
    path = command_line%flag_value("--path")
    if (len(path) == 0) call print_usage_info_and_stop
    print '(*(a))', "Writing test-suite scaffolding in " // path
    call test_suite%write_driver(path // "/driver.f90")
  end associate

contains

    logical function help_requested()
      character(len=:), allocatable :: file_name
      type(command_line_t) command_line
      help_requested = command_line%argument_present([character(len=len("--help"))::"--help","-h"])
    end function

    subroutine print_usage_info_and_stop
      character(len=*), parameter :: usage = &
        new_line('') // new_line('') //      &
        'Usage: fpm run scaffold -- [--subjects <string> --path <string>] | [--help] | -h]' // &
        new_line('') // new_line('') //      &
        'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
        'angular brackets (<>) denote a user-provided value, the --subjects string names a JSON file,'      // new_line('') // &
        'and the --path string names a directory for the new test-suite scaffold.'                          // new_line('')
      stop usage
    end subroutine

end program scaffold
