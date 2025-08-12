! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
#include "julienne-assert-macros.h"

submodule(julienne_compiler_m) compiler_identity_s
  use iso_fortran_env ,only : compiler_version
  use julienne_m, only : call_julienne_assert_, operator(.isAtLeast.), operator(.all.)
  use assert_m
  implicit none

contains

  module procedure inspect_compiler
    character(len=:), allocatable :: compiler_identity

    compiler_identity = compiler_version()

    if (index(compiler_identity, "GCC")==1) then
      compiler%name_ = "gfortran"
      associate(               final_dot => index(compiler_identity                     ,"." ,back=.true.))
        associate(       penultimate_dot => index(compiler_identity(:final_dot-1)       ,"." ,back=.true.))
          associate(space_before_version => index(compiler_identity(:penultimate_dot-1) ," " ,back=.true.))
            associate( &
               major_string => compiler_identity(space_before_version+1 : penultimate_dot-1) &
              ,minor_string => compiler_identity(     penultimate_dot+1 :       final_dot-1) &
              ,patch_string => compiler_identity(           final_dot+1 :                  ) &
            )
              read(major_string, '(i2)') compiler%major_version_
              read(minor_string, '(i1)') compiler%minor_version_
              read(patch_string, '(i1)') compiler%patch_version_
            end associate
          end associate
        end associate
      end associate
    else
       error stop "_____ unrecognized compiler _____"
    end if

    call_assert(allocated(compiler%name_)) 
    call_julienne_assert(.all. ([compiler%major_version_, compiler%major_version_, compiler%major_version_] .isAtLeast. 1))
  end procedure
end submodule compiler_identity_s
