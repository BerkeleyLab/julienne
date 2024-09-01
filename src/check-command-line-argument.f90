! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
program check_command_line_argument
  !! This program serves the dual purposes of 
  !! 1. Showing how to use the command_line_t derived type to check whether a
  !!    command-line argument is present and
  !! 2. Supporting the test suite verification of this same behavior.
  !!
  !! Running this program as follows with the command
  !!
  !! fpm run --example check-command-line-argument -- --some-argument 
  !!
  !! should result in normal termination.
  use assert_m, only : assert
  use julienne_m, only : command_line_t
  implicit none

  type(command_line_t) command_line
  logical argument_passed

  argument_passed = command_line%argument_present(["--some-argument"])

  call assert(argument_passed, "check_command_line_argument: argument present")
end program
