! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_command_line_m
  !! return command line argument information
  implicit none

  private
  public :: command_line_t

  type command_line_t
  contains
    procedure, nopass :: argument_present
    procedure, nopass :: flag_value
  end type

  interface

    module function argument_present(acceptable_argument) result(found)
      implicit none
      !! result is .true. only if a command-line argument matches an element of this function's argument
      character(len=*), intent(in) :: acceptable_argument(:)
        !! sample list: [character(len=len(<longest_argument>)):: "--benchmark", "-b", "/benchmark", "/b"]
        !! where dashes support Linux/macOS, slashes support Windows, and <longest_argument> must be replaced
        !! by the longest list element ("--benchmark" above)
      logical found
    end function

    module function flag_value(flag)
      !! result is the value passed adjacent to a command-line flag
      implicit none
      character(len=*), intent(in) :: flag
      character(len=:), allocatable :: flag_value
    end function

  end interface

end module
