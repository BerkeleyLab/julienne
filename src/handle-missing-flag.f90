! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
program handle_missing_flag
  !! This program serves the dual purposes of
  !!
  !! 1. Showing an example of a command-line with an expected flag missing an
  !! 2. Supporting the test suite check that the returned value has zero length.
  !!
  !! Running this program as follows with the command
  !!
  !! fpm run --example handle-missing-flag -- --empty-flag
  !!
  !! should result in normal termination.
  use assert_m, only : assert
  use julienne_m, only : command_line_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: flag_value
  character(len=*), parameter :: expected_name=""

  flag_value = command_line%flag_value("--empty-flag")

  call assert(flag_value==expected_name,"handle_missing_flag: expected empty flag value", flag_value)
end program
