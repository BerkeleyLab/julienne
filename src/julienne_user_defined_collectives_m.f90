!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module julienne_user_defined_collectives_m
  !! User-defined collective subroutines.
  implicit none

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
