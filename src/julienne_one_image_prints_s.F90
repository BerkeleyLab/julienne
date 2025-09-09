! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_one_image_prints_m) julienne_one_image_prints_s
  implicit none

  integer, parameter :: printing_image = 1

contains

  module procedure print_string
#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image())
#else
    associate(me => 1)
#endif
      if (printing_image == me) then
        select rank(string)
        rank(0)
          print '(a)', string%string()
        rank(1)
          block
            integer line
            do line = 1, size(string)
              print '(a)', string(line)%string()
            end do
          end block
        rank default
          error stop "print_string: unsupported rank"
        end select
      end if
    end associate
  end procedure

  module procedure print_character
#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image())
#else
    associate(me => 1)
#endif
      if (printing_image == me) then
        select rank(character_string)
        rank(0)
          print '(a)', character_string
        rank(1)
          block
            integer line
            do line = 1, size(character_string)
              print '(a)', character_string(line)
            end do
          end block
        rank default
          error stop "print_character: unsupported rank"
        end select
      end if
    end associate
  end procedure

end submodule julienne_one_image_prints_s
