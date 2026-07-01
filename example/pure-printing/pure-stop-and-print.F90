! Copyright (c) 2024-2026, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

program pure_stop_and_print
#if HAVE_STOP_AND_PRINT_SUPPORT
  !! Demonstrate Julienne's support for printing during error termination inside pure procedures
  use julienne_m, only : &
     command_line_t &
    ,file_t &
    ,stop_and_print &
    ,string_t
  use write_stuff_m, only : write_stuff_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: stop_code

  stop_code = usage_info()
  if (      command_line%string_argument_present( [string_t("--help"), string_t("-h")                                 ] ))       stop stop_code
  if (.not. command_line%string_argument_present( [string_t("--file"), string_t("--array"), string_t("--derived-type")] )) error stop stop_code

  associate(file_name => command_line%flag_value("--file"))
    if (len(file_name) > 0) call pure_subroutine(.false., .false., file_t(file_name))
  end associate
  call pure_subroutine(command_line%argument_present(["--array"]), command_line%argument_present(["--derived-type"]))

contains

  pure subroutine pure_subroutine(print_array, print_derived_type, file)
    logical, intent(in) :: print_array, print_derived_type
    type(file_t), intent(in), optional :: file
    if (present(file))      call stop_and_print(header = "______________", data = file, footer = "______________")
    if (print_array)        call stop_and_print(reshape([111,211,121,221, 112,212,122,222], [2,2,2]))
    if (print_derived_type) call stop_and_print(write_stuff_t())
  end subroutine

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

#else
  error stop "Julienne's stop_and_print feature is not supported on this compiler"
#endif
end program pure_stop_and_print
