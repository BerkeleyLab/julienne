module julienne_bin_m
  !! distribute item numbers across bins such that the number of items differs by at most 1 between any two bins
  implicit none

  private
  public :: bin_t

  type bin_t
    !! encapsulate a range of item numbers associated with a bin 
    private
    integer  first_, last_
  contains
    procedure first
    procedure last
  end type

  interface bin_t

    elemental module function construct(num_items, num_bins, bin_number) result(bin)
      !! the result is a bin associated with a range of item numbers
      integer, intent(in) :: num_items, num_bins, bin_number
      type(bin_t) bin
    end function

  end interface

  interface

    elemental module function first(self) result(first_item_number)
      !! the result is the first item number associated with the given bin
      implicit none
      class(bin_t), intent(in) :: self
      integer first_item_number
    end function

    elemental module function last(self) result(last_item_number)
      !! the result is the last item number associated with the given bin
      implicit none
      class(bin_t), intent(in) :: self
      integer last_item_number
    end function

  end interface

end module julienne_bin_m
