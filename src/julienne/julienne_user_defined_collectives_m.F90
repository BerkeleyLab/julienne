! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

#if HAVE_MULTI_IMAGE_SUPPORT

module julienne_user_defined_collectives_m
  !! User-defined collective procedures.
  implicit none

#if HAVE_CO_MAX_CHARACTER_ARRAYE_SUPPORT

  interface co_gather

    module function co_gather_character_array(my_string) result(all_strings)
      implicit none
      character(len=*), intent(in) :: my_string
      character(len=:), allocatable :: all_strings(:)
    end function

  end interface

#endif

  interface

    impure elemental module subroutine co_all(boolean)
      !! If any image in a team calls this subroutine, then every image in the 
      !! the same team must call this subroutine.  This subroutine sets the
      !! "boolean" argument .true. if it is true in all participating images
      !! upon entry and .false. otherwise.
      implicit none
      logical, intent(inout) :: boolean
    end subroutine

  end interface

end module julienne_user_defined_collectives_m

#endif
