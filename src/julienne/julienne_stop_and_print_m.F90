! Copyright (c), The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_stop_and_print_m
  !! Define a pure subroutine that formats and prints various data types during error termination
  use julienne_string_m, only : string_t
  implicit none
  
  private
  public :: stop_and_print
  public :: writable_t
  public :: character_stop_code
  
  type, abstract :: writable_t
    private
    integer :: maxlen_ = 16384
  contains
    generic :: write(formatted) => write_formatted
    procedure(write_formatted_i), deferred :: write_formatted
    procedure :: set_maxlen
    procedure :: maxlen
  end type

  abstract interface

    subroutine write_formatted_i(self, unit, edit_descriptor, v_list, iostat, iomsg)
      import writable_t
      class(writable_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: edit_descriptor
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

  end interface

  interface stop_and_print

    pure module subroutine stop_and_print_string(message)
      implicit none
      type(string_t), intent(in) :: message
    end subroutine

    pure module subroutine stop_and_print_header_and_data(header, data)
      implicit none
      character(len=*), intent(in) :: header
      class(*), intent(in) :: data
    end subroutine

  end interface

  interface

    pure module subroutine set_maxlen(self, length)
      implicit none
      class(writable_t), intent(inout) :: self
      integer, intent(in) :: length
    end subroutine

    pure module function maxlen(self) result(length)
      implicit none
      class(writable_t), intent(in) :: self
      integer length
    end function

    pure module function character_stop_code(stuff) result(stop_code)
      implicit none
      class(*), intent(in) :: stuff(..)
      character(len=:), allocatable :: stop_code
    end function

  end interface

end module julienne_stop_and_print_m
