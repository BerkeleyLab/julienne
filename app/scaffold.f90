! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program scaffold
  use julienne_m, only : command_line_t, file_t, test_suite_t
  implicit none
  type(command_line_t) command_line
  integer i

  if (help_requested()) call print_usage_info_and_stop

#if ! defined(__GCC__)
  associate(subjects_file_name => command_line%flag_value("--json-file"))
    if (len(subjects_file_name) == 0) call print_usage_info_and_stop
    print '(*(a))', "Reading test subject information from " // subjects_file_name
    associate(test_suite => test_suite_t(file_t(subjects_file_name)))
      associate(path => command_line%flag_value("--suite-path"))
        print '(*(a))', "Writing test-suite scaffolding in " // path
        if (len(path) == 0) call print_usage_info_and_stop
        associate(driver => test_suite%driver_file())
          call driver%write_lines(path // "/driver.f90")
        end associate
        associate(subjects => test_suite%test_subjects(), modules => test_suite%test_modules())
          do i = 1, size(subjects)
            associate(stub => test_suite%stub_file(subjects(i)))
              call stub%write_lines(path // "/" // modules(i) // ".f90")
            end associate
          end do
        end associate
      end associate
    end associate
  end associate
#else
  block
    character(len=:), allocatable ::  path, subjects_file_name
    type(file_t) driver
    subjects_file_name = command_line%flag_value("--json-file")
    if (len(subjects_file_name) == 0) call print_usage_info_and_stop
    print '(*(a))', "Reading test subject information from " // subjects_file_name
    associate(test_suite => test_suite_t(file_t(subjects_file_name)))
      path = command_line%flag_value("--suite-path")
      if (len(path) == 0) call print_usage_info_and_stop
      print '(*(a))', "Writing test-suite scaffolding in " // path
      call test_suite%write_driver(path // "/driver.f90")
    end associate
  end block
#endif

contains

    logical function help_requested()
      character(len=:), allocatable :: file_name
      type(command_line_t) command_line
      help_requested = command_line%argument_present([character(len=len("--help"))::"--help","-h"])
    end function

    subroutine print_usage_info_and_stop
      character(len=*), parameter :: usage = &
        new_line('') // new_line('') //      &
        'Usage: fpm run scaffold -- [--json-file <string> --suite-path <string>] | [--help] | -h]' // &
        new_line('') // new_line('') //      &
        'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
        'angular brackets (<>) denote a user-provided value, the --subjects string names a JSON file,'      // new_line('') // &
        'and the --path string names a directory for the new test-suite scaffold.'                          // new_line('')
      stop usage
    end subroutine

end program scaffold
