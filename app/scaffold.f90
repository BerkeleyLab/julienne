! Copyrigh (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program generate_driver
  use julienne_m, only : command_line_t, file_t
  use julienne_test_suite_m, only : test_suite_t
  implicit none
  type(command_line_t) command_line

  if (help_requested()) call print_usage_info_and_stop

  associate(input_file_name => command_line%flag_value("--input-file"))
    if (len(input_file_name) == 0) call print_usage_info_and_stop
    associate(test_suite => test_suite_t(file_t(input_file_name)))
      associate(output_file_name => command_line%flag_value("--output-file"))
        if (len(output_file_name) == 0) call print_usage_info_and_stop
        associate(driver_program => test_suite%driver_file())
          call driver_program%write_lines(output_file_name)
        end associate
      end associate
    end associate
  end associate

contains

    logical function help_requested()
      character(len=:), allocatable :: file_name
      type(command_line_t) command_line
      help_requested = command_line%argument_present([character(len=len("--help"))::"--help","-h"])
    end function

    subroutine print_usage_info_and_stop

      associate(command_line => command_line_t())
        block
          character(len=*), parameter :: usage = &
            new_line('') // new_line('') // &
            'Usage: fpm run -- [--input-file <file-name> --output-file <file-name>] | [--help] | -h]' // &
            new_line('') // new_line('') // &
            'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments' // new_line('') // &
            'and angular brackets (<>) denote a user-provided value' // new_line('')

          if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage
        end block
      end associate

      print "(a)", new_line("") // "Append '-- --help' or '-- -h' to your `fpm` command to display usage information."

    end subroutine

end program generate_driver
  !use julienne_m, only : test_fixture_t, test_harness_t
