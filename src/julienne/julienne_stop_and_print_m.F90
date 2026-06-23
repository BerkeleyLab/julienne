! Copyright (c), The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_stop_and_print_m
  !! Define a pure subroutine that terminates with an ERROR STOP message from a string_t
  use julienne_string_m, only : string_t
  implicit none
  
  private
  public :: stop_and_print

contains

  pure subroutine stop_and_print(message)
    implicit none
    type(string_t), intent(in) :: message
    call internal_error_stop(message%string())
  end subroutine
  
end module
