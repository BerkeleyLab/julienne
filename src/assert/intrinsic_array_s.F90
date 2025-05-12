submodule(intrinsic_array_m) intrinsic_array_s
  use assert_m, only : assert
  implicit none

contains

#ifndef _CRAYFTN
  module procedure construct

  select rank(array)
    rank(1)
      select type(array)
      type is(complex)
        allocate(intrinsic_array%complex_1D, source = array)
      type is(complex(kind(1.D0)))
        allocate(intrinsic_array%complex_double_1D, source = array)
      type is(integer)
        allocate(intrinsic_array%integer_1D, source = array)
      type is(logical)
        allocate(intrinsic_array%logical_1D, source = array)
      type is(real)
        allocate(intrinsic_array%real_1D, source = array)
      type is(double precision)
        intrinsic_array%double_precision_1D = array
      class default
        error  stop "intrinsic_array_s(construct): unsupported rank-1 type"
      end select
    rank(2)
      select type(array)
      type is(complex)
        allocate(intrinsic_array%complex_2D, source = array)
      type is(complex(kind(1.D0)))
        allocate(intrinsic_array%complex_double_2D, source = array)
      type is(integer)
        allocate(intrinsic_array%integer_2D, source = array)
      type is(logical)
        allocate(intrinsic_array%logical_2D, source = array)
      type is(real)
        allocate(intrinsic_array%real_2D, source = array)
      type is(double precision)
        allocate(intrinsic_array%double_precision_2D, source = array)
      class default
        error  stop "intrinsic_array_s(construct): unsupported rank-2 type"
      end select

    rank(3)
      select type(array)
      type is(complex)
        allocate(intrinsic_array%complex_3D, source = array)
      type is(complex(kind(1.D0)))
        allocate(intrinsic_array%complex_double_3D, source = array)
      type is(integer)
        allocate(intrinsic_array%integer_3D, source = array)
      type is(logical)
        allocate(intrinsic_array%logical_3D, source = array)
      type is(real)
        allocate(intrinsic_array%real_3D, source = array)
      type is(double precision)
        allocate(intrinsic_array%double_precision_3D, source = array)
      class default
        error  stop "intrinsic_array_s(construct): unsupported rank-3 type"
      end select

    rank default
      error  stop "intrinsic_array_s(construct): unsupported rank"
    end select

  end procedure

#else
    module procedure complex_array

      select rank(array)
        rank(1)
            allocate(intrinsic_array%complex_1D, source = array)
        rank(2)
            allocate(intrinsic_array%complex_2D, source = array)
        rank(3)
            allocate(intrinsic_array%complex_3D, source = array)
        rank default
          error  stop "intrinsic_array_s(complex_array): unsupported rank"
      end select

    end procedure

    module procedure integer_array

      select rank(array)
        rank(1)
            allocate(intrinsic_array%integer_1D, source = array)
        rank(2)
            allocate(intrinsic_array%integer_2D, source = array)
        rank(3)
            allocate(intrinsic_array%integer_3D, source = array)
        rank default
          error  stop "intrinsic_array_s(integer_array): unsupported rank"
      end select

    end procedure

    module procedure logical_array

      select rank(array)
        rank(1)
            allocate(intrinsic_array%logical_1D, source = array)
        rank(2)
            allocate(intrinsic_array%logical_2D, source = array)
        rank(3)
            allocate(intrinsic_array%logical_3D, source = array)
        rank default
          error  stop "intrinsic_array_s(logical_array): unsupported rank"
      end select

    end procedure

    module procedure real_array

      select rank(array)
        rank(1)
            allocate(intrinsic_array%real_1D, source = array)
        rank(2)
            allocate(intrinsic_array%real_2D, source = array)
        rank(3)
            allocate(intrinsic_array%real_3D, source = array)
        rank default
          error  stop "intrinsic_array_s(real_array): unsupported rank"
      end select

    end procedure

    module procedure double_precision_array

      select rank(array)
        rank(1)
            allocate(intrinsic_array%double_precision_1D, source = array)
        rank(2)
            allocate(intrinsic_array%double_precision_2D, source = array)
        rank(3)
            allocate(intrinsic_array%double_precision_3D, source = array)
        rank default
          error  stop "intrinsic_array_s(double_precision_array): unsupported rank"
      end select

    end procedure

#endif

  pure function allocated_components(self) 
    type(intrinsic_array_t), intent(in) :: self
    logical, allocatable :: allocated_components(:)
    allocated_components = [ &
       allocated(self%complex_1D), allocated(self%real_1D), allocated(self%integer_1D), allocated(self%complex_double_1D) &
      ,allocated(self%complex_2D), allocated(self%real_2D), allocated(self%integer_2D), allocated(self%complex_double_2D) &
      ,allocated(self%complex_3D), allocated(self%real_3D), allocated(self%integer_3D), allocated(self%complex_double_3D) &
      ,allocated(self%logical_1D), allocated(self%double_precision_1D) &
      ,allocated(self%logical_2D), allocated(self%double_precision_2D) &
      ,allocated(self%logical_3D), allocated(self%double_precision_3D) &
    ]
  end function

  module procedure as_character
    integer, parameter :: single_number_width=64

    associate(a => allocated_components(self))
      call assert(count(a) == 1, "intrinsic_array_s(as_character): invalid number of allocated components", intrinsic_array_t(a))
    end associate

    if (allocated(self%complex_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_1D))
      write(character_self, *) self%complex_1D
    else if (allocated(self%complex_double_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_double_1D))
      write(character_self, *) self%complex_double_1D
    else if (allocated(self%integer_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%integer_1D))
      write(character_self, *) self%integer_1D
    else if (allocated(self%logical_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%logical_1D))
      write(character_self, *) self%logical_1D
    else if (allocated(self%real_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%real_1D))
      write(character_self, *) self%real_1D
    else if (allocated(self%double_precision_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%double_precision_1D))
      write(character_self, *) self%double_precision_1D
    else if (allocated(self%complex_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_2D))
      write(character_self, *) self%complex_2D
    else if (allocated(self%complex_double_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_double_2D))
      write(character_self, *) self%complex_double_2D
    else if (allocated(self%integer_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%integer_2D))
      write(character_self, *) self%integer_2D
    else if (allocated(self%logical_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%logical_2D))
      write(character_self, *) self%logical_2D
    else if (allocated(self%real_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%real_2D))
      write(character_self, *) self%real_2D
    else if (allocated(self%double_precision_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%double_precision_2D))
      write(character_self, *) self%double_precision_2D
    else if (allocated(self%complex_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_3D))
      write(character_self, *) self%complex_3D
    else if (allocated(self%complex_double_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_double_3D))
      write(character_self, *) self%complex_double_3D
    else if (allocated(self%integer_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%integer_3D))
      write(character_self, *) self%integer_3D
    else if (allocated(self%logical_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%logical_3D))
      write(character_self, *) self%logical_3D
    else if (allocated(self%real_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%real_3D))
      write(character_self, *) self%real_3D
    else if (allocated(self%double_precision_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%double_precision_3D))
      write(character_self, *) self%double_precision_3D
    end if

    character_self = trim(adjustl(character_self))
  end procedure

end submodule intrinsic_array_s
