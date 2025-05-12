module intrinsic_array_m
  !! Define an abstract class that supports object representation in character form
  use characterizable_m, only : characterizable_t
  implicit none

  private
  public :: intrinsic_array_t

  type, extends(characterizable_t) :: intrinsic_array_t
    complex,             allocatable :: complex_1D(:)
    complex(kind(1.D0)), allocatable :: complex_double_1D(:)
    integer,             allocatable :: integer_1D(:)
    logical,             allocatable :: logical_1D(:)
    real,                allocatable :: real_1D(:)
    double precision,    allocatable :: double_precision_1D(:)

    complex,             allocatable :: complex_2D(:,:)
    complex(kind(1.D0)), allocatable :: complex_double_2D(:,:)
    integer,             allocatable :: integer_2D(:,:)
    logical,             allocatable :: logical_2D(:,:)
    real,                allocatable :: real_2D(:,:)
    double precision,    allocatable :: double_precision_2D(:,:)

    complex,             allocatable :: complex_3D(:,:,:)
    complex(kind(1.D0)), allocatable :: complex_double_3D(:,:,:)
    integer,             allocatable :: integer_3D(:,:,:)
    logical,             allocatable :: logical_3D(:,:,:)
    real,                allocatable :: real_3D(:,:,:)
    double precision,    allocatable :: double_precision_3D(:,:,:)
  contains
    procedure :: as_character
  end type

  interface intrinsic_array_t
    
#ifndef _CRAYFTN

    pure module function construct(array) result(intrinsic_array)
      implicit none
      class(*), intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function

#else

    pure module function complex_array(array) result(intrinsic_array)
      implicit none
      complex, intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function

    pure module function integer_array(array) result(intrinsic_array)
      implicit none
      integer, intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function

    pure module function logical_array(array) result(intrinsic_array)
      implicit none
      logical, intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function

    pure module function real_array(array) result(intrinsic_array)
      implicit none
      real, intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function

    pure module function double_precision_array(array) result(intrinsic_array)
      implicit none
      double precision, intent(in) :: array(..)
      type(intrinsic_array_t) intrinsic_array
    end function

#endif
    
  end interface

  interface
    
    pure module function as_character(self) result(character_self)
      implicit none
      class(intrinsic_array_t), intent(in) :: self
      character(len=:), allocatable :: character_self
    end function
    
  end interface

end module intrinsic_array_m
