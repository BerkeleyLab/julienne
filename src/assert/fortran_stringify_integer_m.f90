module fortran_stringify_integer_m
  implicit none

contains  

  pure function fortran_stringify_integer(number) result(number_as_string)
    integer, intent(in) :: number
    integer, parameter :: max_len=128
    character(len=max_len) :: untrimmed_string
    character(len=:), allocatable :: number_as_string

    write(untrimmed_string, '(i0)') number
    number_as_string = trim(adjustl(untrimmed_string))
  end function

end module
