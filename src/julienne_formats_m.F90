! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_formats_m
  !! Useful strings for formatting `print` and `write` statements
  implicit none

  character(len=*), parameter :: csv = "(*(G0,:,','))" !! comma-separated values
  character(len=*), parameter :: cscv = "(*('(',G0,',',G0,')',:,',')))" !! comma-separated complex values

#ifndef _CRAYFTN

  interface

    pure module function separated_values(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator 
      class(*), intent(in) :: mold(..)
      character(len=:), allocatable :: format_string
    end function

  end interface

#else

  interface separated_values

    pure module function separated_values_1D(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator 
      class(*), intent(in) :: mold(:)
      character(len=:), allocatable :: format_string
    end function

  end interface

#endif

end module julienne_formats_m
