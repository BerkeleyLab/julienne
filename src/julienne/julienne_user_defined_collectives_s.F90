! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_user_defined_collectives_m) julienne_user_defined_collectives_s
  implicit none

contains

  module procedure co_all
#if HAVE_MULTI_IMAGE_SUPPORT
    integer binary
    binary = merge(0, 1, boolean)
    call co_max(binary, result_image)
    boolean = merge(.true., .false., binary==0)
#endif
  end procedure

end submodule julienne_user_defined_collectives_s
