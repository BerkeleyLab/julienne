! Copyright (c) 2024-2026, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-language-support.F90"

submodule(write_stuff_m) write_stuff_s
#if HAVE_STOP_AND_PRINT_SUPPORT
  implicit none

contains

  module procedure  write_formatted
    write(unit,'(a)'     ) new_line('')
    write(unit,'(a)'     ) "write_stuff_t {" // new_line('')
    write(unit,'(a,i2,a)') "  answer = ", self%answer_, new_line('')
    write(unit,'(a)'     ) "}" // new_line('')
    iostat = 0
  end procedure
#else
  EMPTY_MODULE_SUBMODULE
#endif
end submodule
