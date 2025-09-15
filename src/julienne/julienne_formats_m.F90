! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_formats_m
  !! Useful strings for formatting `print` and `write` statements
  implicit none

  character(len=*), parameter :: csv = "(*(G25.20,:,','))" !! comma-separated values
  character(len=*), parameter :: cscv = "(*('(',G25.20,',',G25.20,')',:,',')))" !! comma-separated complex values

  interface

    pure module function separated_values(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator 
      class(*), intent(in) :: mold(..)
      character(len=:), allocatable :: format_string
    end function

  end interface

end module julienne_formats_m
