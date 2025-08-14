! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module specimen_m
  !! Example test specimen corresponding to the test defined in specimen_test_m.F90
  implicit none

  type specimen_t
  contains
    procedure pi
  end type

contains

  function pi(self)
    class(specimen_t), intent(in) :: self
    real pi
    associate(avoid_unused_variable_warning => self)
    end associate
    pi = 3.1415926536
  end function

end module