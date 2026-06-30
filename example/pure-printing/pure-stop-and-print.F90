! Copyright (c) 2024-2026, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program pure_stop_and_print
  !! Demonstrate Julienne's support for printing during error termination inside pure procedures
  use julienne_m, only : &
     command_line_t &
    ,file_t &
    ,stop_and_print &
    ,string_t
  use write_stuff_m, only : write_stuff_t
  implicit none

  type(command_line_t) command_line

  if (      command_line%argument_present( [string_t("--help"), string_t("-h")                                 ] ))       stop usage_info()
  if (.not. command_line%argument_present( [string_t("--file"), string_t("--array"), string_t("--derived-type")] )) error stop usage_info()

  associate(file_name => command_line%flag_value("--file"))
    if (len(file_name) > 0) call stop_and_print(header = "___" // file_name // "___", data = file_t(file_name), footer = "________")
  end associate

  associate(array => reshape([111,211,121,221, 112,212,122,222], [2,2,2]))
    if (command_line%argument_present(["--array"])) call stop_and_print(array)
  end associate

  if (command_line%argument_present(["--derived-type"])) call stop_and_print(write_stuff_t())

contains

  pure function usage_info() result(message)
    character(len=:), allocatable :: message
    message = new_line('') // new_line('') &
      // 'Usage:' // new_line('') // new_line('') &
      // '  fpm run \'  // new_line('') &
      // '     --example pure-stop-and-print \'  // new_line('') &
      // '     --compiler flang --profile release \' // new_line('') &
      // '     -- [-h|--help] | [--file <name>] | [--array] | [--derived-type]' // new_line('') // new_line('') &
      // 'where pipes (|) separate alternatives, square brackets ([]) delimit' // new_line('') &
      // 'optional arguments, and angular brackets (<>) delimit user input.' // new_line('')
   end function

end program pure_stop_and_print
