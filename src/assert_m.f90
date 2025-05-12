module assert_m
  !! Public interface
  use intrinsic_array_m,   only : intrinsic_array_t
  use assert_subroutine_m, only : assert, assert_always
  use characterizable_m,   only : characterizable_t

  ! The function below is public only to support automated
  ! invocation via `assert_macros.h`.  For a more broadly useful
  ! function to convert numeric data to string format, please
  ! consider using the functions that can be accessed via the 
  ! `string_t` generic interface in the Julienne framework at
  ! https://go.lbl.gov/julienne.
  use fortran_stringify_integer_m, only : fortran_stringify_integer
  implicit none
end module
