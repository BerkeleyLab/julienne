! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module widget_m
  !! Example test widget corresponding to the test defined in widget_test_m.F90
  implicit none

  type widget_t
  contains
    procedure pi
  end type

contains

  function pi(self)
    class(widget_t), intent(in) :: self
    real pi
    associate(avoid_unused_variable_warning => self)
    end associate
    pi = 3.1415926536
  end function

end module