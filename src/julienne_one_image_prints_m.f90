! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_one_image_prints_m
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: one_image_prints

  interface one_image_prints

    module subroutine print_string(string)
      implicit none
      type(string_t), intent(in) :: string(..)
    end subroutine

    module subroutine print_character(character_string)
      implicit none
      character(len=*), intent(in) :: character_string(..)
    end subroutine

  end interface

end module julienne_one_image_prints_m
