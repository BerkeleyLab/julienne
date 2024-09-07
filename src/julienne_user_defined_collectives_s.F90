!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
#ifdef __flang__
  #define NO_MULTI_IMAGE_SUPPORT
#endif
submodule(julienne_user_defined_collectives_m) julienne_user_defined_collectives_s
  implicit none

contains

  module procedure co_all
#ifndef NO_MULTI_IMAGE_SUPPORT
    call co_reduce(boolean, both)
#endif
  contains
    pure function both(lhs,rhs) result(lhs_and_rhs)
      logical, intent(in) :: lhs,rhs
      logical lhs_and_rhs
      lhs_and_rhs = lhs .and. rhs
    end function
  end procedure

end submodule julienne_user_defined_collectives_s
