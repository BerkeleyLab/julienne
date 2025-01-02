! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module specimen_m
  !! Example test specimen corresponding to the test defined in specimen_test_m.F90
  implicit none

  type specimen_t
  contains
    procedure, nopass :: zero
  end type

contains

  pure function zero() result(incorrect_value)
    integer incorrect_value
    incorrect_value = 1
  end function

end module