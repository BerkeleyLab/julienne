! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_command_line_m) julienne_command_line_s
  implicit none

contains

  module procedure argument_present
      !! list of acceptable arguments
      !! sample list: [character(len=len(longest_argument)):: "--benchmark", "-b", "/benchmark", "/b"]
      !! where dashes support Linux/macOS and slashes support Windows
    integer :: i, argnum, arglen
      !! loop counter, argument position, argument length
    character(len=32) arg
      !! argument position

      !! acceptable argument lengths (used to preclude extraneous trailing characters)

    associate(acceptable_length => [(len(trim(acceptable_argument(i))), i = 1, size(acceptable_argument))])

      found = .false.

      do argnum = 1,command_argument_count()

        call get_command_argument(argnum, arg, arglen)

        if (any( &
          [(arg==acceptable_argument(i) .and. arglen==acceptable_length(i), i = 1, size(acceptable_argument))] &
        )) then
          found = .true.
        end if

      end do

    end associate

  end procedure

  module procedure flag_value

    integer argnum, arglen, flag_value_length
    character(len=:), allocatable :: arg

    associate(argcount => command_argument_count())
      if (argcount==0) then
        flag_value=""
      else
        flag_search: &
        do argnum = 1,argcount

          if (allocated(arg)) deallocate(arg)

          call get_command_argument(argnum, length=arglen)
          allocate(character(len=arglen) :: arg)
          call get_command_argument(argnum, arg)

          if (arg==flag) then
            call get_command_argument(argnum+1, length=flag_value_length)
            allocate(character(len=flag_value_length) :: flag_value)
            call get_command_argument(argnum+1, flag_value)
            exit flag_search
          end if
        end do flag_search
      end if
    end associate

  end procedure

end submodule
