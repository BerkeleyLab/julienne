! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module specimen_m
  !! Example test specimen corresponding to the test defined in specimen_test_m.F90
  implicit none

  type specimen_t
  contains
    procedure, nopass :: zero
    procedure, nopass :: one
  end type

contains

  pure function zero() result(correct_value)
    real correct_value
    correct_value = 0
  end function

  pure function one() result(correct_value)
    integer correct_value
    correct_value = 1
  end function

end module