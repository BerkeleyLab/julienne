! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
program get_flag_value
  !! Demonstrate how to find the value of a command-line flag 
  !! Running this program as follows with the command
  !!
  !! fpm run --example get-flag-value -- --input-file foo
  !!
  !! result in normal termination.
  use assert_m, only : assert
  use julienne_m, only : command_line_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: input_file_name
  character(len=*), parameter :: expected_name="some_file_name"

  input_file_name = command_line%flag_value("--input-file")

  call assert(input_file_name==expected_name,"get_flag_value: input_file_name==expected_name",diagnostic_data=input_file_name )
end program
