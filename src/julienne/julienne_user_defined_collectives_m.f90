! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_user_defined_collectives_m
  !! User-defined collective procedures.
  implicit none

  interface co_gather

    module function co_gather(my_string) result(all_strings)
      implicit none
      character(len=*), intent(in) :: my_string
      character(len=:), allocatable :: all_strings(:)
    end function

  end interface

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
