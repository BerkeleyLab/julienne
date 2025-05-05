! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
program check_for_command_line_argument
  !! This program shows how to use the command_line_t derived type to check whether a 
  !! command-line argument is present.  Running this program as follows with the command
  !! should print an indication that the command-line argument is present:
  !!
  !!   fpm run --example check-for-command-line-argument -- --some-argument
  !!
  !! Running the program without the argument or with the argument spelled differently
  !! should print an indication that the argument is not present:
  !!
  !!   fpm run --example check-for-command-line-argument
  use julienne_m, only : command_line_t
  implicit none

  type(command_line_t) command_line

  if (command_line%argument_present(["--some-argument"])) then
     print '(a)', new_line('') // "argument 'some-argument' present" // new_line('')
  else
     print '(a)', new_line('') // "argument 'some-argument' not present" // new_line('')
  end if
end program
