! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

#if HAVE_MULTI_IMAGE_SUPPORT
submodule(julienne_user_defined_collectives_m) julienne_user_defined_collectives_s
  implicit none

contains

#if HAVE_CO_MAX_CHARACTER_ARRAY_SUPPORT
  module procedure co_gather

    integer i, max_len
    character(len=:), allocatable :: array(:)

    associate(me => this_image())
      max_len = len(my_string)
      call co_max(max_len)
      all_strings = &
        [ character(len=max_len) :: &
           [( "", i = 1, me-1      )] &
          ,my_string &
          ,[( "", i = me+1, num_images() )] &
        ]
      call co_max(all_strings)
    end associate
  end procedure
#endif

  module procedure co_all
    integer binary
    binary = merge(0, 1, boolean)
    call co_max(binary)
    boolean = merge(.true., .false., binary==0)
  end procedure

end submodule julienne_user_defined_collectives_s
#endif
