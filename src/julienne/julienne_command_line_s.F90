! Copyright (c) 2024-2026, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

submodule(julienne_command_line_m) julienne_command_line_s
  implicit none

contains

  module procedure string_argument_present
     integer a
     integer maxlen

     maxlen = maxval([(len(acceptable_argument(a)%string()), a = 1,size(acceptable_argument))])
     found = argument_present( &
        [( [character(len=maxlen) :: acceptable_argument(a)%string()], a = 1, size(acceptable_argument))] &
     )
# ifdef __INTEL_COMPILER
  ! workaround ifx bug where it thinks argument to len must be a constant expression
  contains
    pure function len(char) result(l)
      character(len=*), intent(in) :: char
      integer :: l
      block
        intrinsic :: len
        l = len(char)
      end block
    end function
# endif
  end procedure

  module procedure argument_present ! specific procedure for character argument
      !! list of acceptable arguments
      !! sample list: [character(len=len(longest_argument)):: "--benchmark", "-b", "/benchmark", "/b"]
      !! where dashes support Linux/macOS and slashes support Windows
    integer :: i, argnum, arglen
      !! loop counter, argument position, argument length
    character(len=32) arg
      !! argument position

      !! acceptable argument lengths (used to preclude extraneous trailing characters)

    associate(acceptable_length => [(len(trim(acceptable_argument(i))), i = 1, size(acceptable_argument))])

      do argnum = 1,command_argument_count()

        call get_command_argument(argnum, arg, arglen)

        if (any( &
          [(arg==acceptable_argument(i) .and. arglen==acceptable_length(i), i = 1, size(acceptable_argument))] &
        )) then
          found = .true.
          return
        end if

      end do

      found = .false.

    end associate

  end procedure

  module procedure string_flag_value
    value = flag_value(flag%string())
  end procedure

  module procedure flag_value ! specific procedure for character argument
    integer argnum, arglen, value_length
    character(len=:), allocatable :: arg

    do argnum = 1,command_argument_count()-1
      call get_command_argument(argnum, length=arglen)
      allocate(character(len=arglen) :: arg)
      call get_command_argument(argnum, arg)
      if (arg==flag) then
        call get_command_argument(argnum+1, length=value_length)
        allocate(character(len=value_length) :: value)
        call get_command_argument(argnum+1, value)
        return
      end if
      deallocate(arg)
    end do
    value=""
  end procedure

end submodule
