! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
program get_command_line_flag_value
  !! This program demonstrates how to find the value of a command-line flag.  Running this program
  !! as follows should print 'foo=bar' without quotes:
  !!
  !! fpm run --example get-command-line-flag-value -- --foo bar
  !!
  !! Running the above command either without `bar` or without "--foo bar" should print an indication the message "flag '--foo' not present or present with no value".
  !! was provided.
  use julienne_m, only : command_line_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: foo_value

  foo_value = command_line%flag_value("--foo")

  if (len(foo_value)/=0) then 
    print '(a)', new_line('') // "foo=" // foo_value // new_line('')
  else
    print '(a)', new_line('') // "flag '--foo' not present or present with no value" // new_line('')
  end if
end program
