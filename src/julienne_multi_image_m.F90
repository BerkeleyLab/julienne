! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_multi_image_m
  !! Define interfaces for supporting multi-image execution
  implicit none

  public :: internal_this_image, internal_num_images
  public :: internal_co_sum_integer
  public :: internal_error_stop

  interface

    module function internal_this_image() result(this_image_id)
      implicit none
      integer :: this_image_id
    end function

    module function internal_num_images() result(image_count)
      implicit none
      integer :: image_count
    end function

    module subroutine internal_sync_all()
      implicit none
    end subroutine

    module subroutine internal_co_sum_integer(a, result_image)
      implicit none
      integer, intent(inout), target :: a(:)
      integer, intent(in), optional :: result_image
    end subroutine

    module subroutine internal_error_stop(stop_code_char)
      implicit none
      character(len=*), intent(in) :: stop_code_char
    end subroutine

  end interface

#if JULIENNE_PARALLEL_CALLBACKS
    public :: julienne_this_image_interface,     julienne_this_image
    public :: julienne_num_images_interface,     julienne_num_images
    public :: julienne_sync_all_interface,       julienne_sync_all
    public :: julienne_co_sum_integer_interface, julienne_co_sum_integer
    public :: julienne_error_stop_interface,     julienne_error_stop

    abstract interface
      function julienne_this_image_interface() result(this_image_id)
        implicit none
        integer :: this_image_id
      end function
    end interface
    procedure(julienne_this_image_interface), pointer :: julienne_this_image

    abstract interface
      function julienne_num_images_interface() result(image_count)
        implicit none
        integer :: image_count
      end function
    end interface
    procedure(julienne_num_images_interface), pointer :: julienne_num_images

    abstract interface
      subroutine julienne_sync_all_interface()
        implicit none
      end subroutine
    end interface
    procedure(julienne_sync_all_interface), pointer :: julienne_sync_all

    abstract interface
      subroutine julienne_co_sum_integer_interface(a, result_image)
        implicit none
        integer, intent(inout), target :: a(:)
        integer, intent(in), optional :: result_image
      end subroutine
    end interface
    procedure(julienne_co_sum_integer_interface), pointer :: julienne_co_sum_integer

    abstract interface
      subroutine julienne_error_stop_interface(stop_code_char)
        implicit none
        character(len=*), intent(in) :: stop_code_char
      end subroutine
    end interface
    procedure(julienne_error_stop_interface), pointer :: julienne_error_stop
#endif


end module julienne_multi_image_m
