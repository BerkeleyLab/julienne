! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_compiler_m
  !! Compiler identity abstraction
  use iso_fortran_env ,only : compiler_version
  implicit none

  private
  public :: compiler_t

  type compiler_t
    private
    character(len=:), allocatable :: name_
    integer :: major_version_, minor_version_, patch_version_
  end type

  interface compiler_t

    module function inspect_compiler() result(compiler)
      implicit none
      type(compiler_t) compiler
    end function

  end interface 

end module julienne_compiler_m
