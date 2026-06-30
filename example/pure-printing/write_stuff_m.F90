! Copyright (c) 2024-2026, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module write_stuff_m
  !! Demonstrate a derived type that is writable to a stop  via Julienne's stop_and_print utility
  use julienne_m, only : writable_t
  implicit none

  type, extends(writable_t) :: write_stuff_t
    integer :: answer_ = 42
  contains
    procedure :: write_formatted
  end type

  interface

    module subroutine write_formatted(self, unit, edit_descriptor, v_list, iostat, iomsg)
      class(write_stuff_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: edit_descriptor
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

  end interface

end module
