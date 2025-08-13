! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_test_suite_m
  use julienne_file_m, only : file_t
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: test_suite_t

  type test_suite_t
    private
    type(string_t), allocatable :: test_subjects_(:)
  contains
    procedure to_file
    procedure driver_file
    procedure write_driver
  end type

  interface test_suite_t

    pure module function from_components(test_subjects) result(test_suite)
      implicit none
      type(string_t), intent(in) :: test_subjects(:)
      type(test_suite_t) test_suite
    end function

    pure module function from_file(file) result(test_suite)
      implicit none
      type(file_t), intent(in) :: file
      type(test_suite_t) test_suite
    end function

  end interface

  interface

    pure module function to_file(self) result(file)
      implicit none
      class(test_suite_t), intent(in) :: self
      type(file_t) file
    end function

    pure module function driver_file(self) result(file)
      implicit none
      class(test_suite_t), intent(in) :: self
      type(file_t) file
    end function

    module subroutine write_driver(self, file_name)
      implicit none
      class(test_suite_t), intent(in) :: self
      character(len=*), intent(in) :: file_name
    end subroutine

  end interface

end module julienne_test_suite_m
