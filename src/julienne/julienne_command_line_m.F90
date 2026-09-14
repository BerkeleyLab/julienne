! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_command_line_m
  !! return command line argument information
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: command_line_t

  type command_line_t
  contains
    generic :: argument_present => character_argument_present, string_argument_present
    procedure, nopass :: character_argument_present, string_argument_present
    generic :: flag_value => character_flag_value, string_flag_value
    procedure, nopass :: character_flag_value, string_flag_value
  end type

  interface

    module function character_argument_present(acceptable_argument) result(found)
      implicit none
      !! result is .true. only if a command-line argument matches an element of this function's argument
      character(len=*), intent(in) :: acceptable_argument(:)
        !! sample list: [character(len=len(<longest_argument>)):: "--benchmark", "-b", "/benchmark", "/b"]
        !! where dashes support Linux/macOS, slashes support Windows, and <longest_argument> must be replaced
        !! by the longest list element ("--benchmark" above)
      logical found
    end function

    module function string_argument_present(acceptable_argument) result(found)
      implicit none
      !! same as `character_argument_present` but allowing ragged-edged array of character values
      type(string_t), intent(in) :: acceptable_argument(:)
      logical found
    end function

    module function character_flag_value(flag) result(value)
      !! result = { the value passed immediately after a command-line flag if the flag is present or
      !!          { an empty string otherwise.
      implicit none
      character(len=*), intent(in) :: flag
      character(len=:), allocatable :: value
    end function

    module function string_flag_value(flag) result(value)
      !! same as `character_flag_value` but accepting a string_t dummy argument
      implicit none
      type(string_t), intent(in) :: flag
      character(len=:), allocatable :: value
    end function

  end interface

end module
