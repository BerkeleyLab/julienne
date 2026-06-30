! Copyright (c) 2024-2026, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(write_stuff_m) write_stuff_s
  implicit none

contains

  module procedure  write_formatted
    write(unit,'(a)'     ) new_line('')
    write(unit,'(a)'     ) "write_stuff_t {" // new_line('')
    write(unit,'(a,i2,a)') "  answer = ", self%answer_, new_line('')
    write(unit,'(a)'     ) "}" // new_line('')
  end procedure

end submodule
