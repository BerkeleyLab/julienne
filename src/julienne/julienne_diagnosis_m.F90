! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_diagnosis_m
  !! Define an abstraction for describing test outcomes and diagnostic information
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: diagnosis_t

  type diagnosis_t
    !! Encapsulate test outcome and diagnostic information
    private
    logical passed_
    character(len=:), allocatable :: diagnostics_
  contains
    procedure passed
    procedure diagnostics
  end type

  interface diagnosis_t

    elemental module function construct_from_string_t(passed, diagnostics) result(diagnosis)
      !! The result is a diagnosis_t object with the components defined by the dummy arguments
      implicit none
      logical, intent(in) :: passed
      type(string_t), intent(in) :: diagnostics
      type(diagnosis_t) diagnosis
    end function

    elemental module function construct_from_character(passed, diagnostics) result(diagnosis)
      !! The result is a diagnosis_t object with the components defined by the dummy arguments
      implicit none
      logical, intent(in) :: passed
      character(len=*), intent(in) :: diagnostics
      type(diagnosis_t) diagnosis
    end function

  end interface

  interface

    elemental module function passed(self) result(test_passed)
      !! The result is .true. if the test passed
      implicit none
      class(diagnosis_t), intent(in) :: self
      logical test_passed
    end function

    elemental module function diagnostics(self) result(diagnostics_string)
      !! The result is a diagnostic string describing a failed test or a zero-length string if the test passed
      implicit none
      class(diagnosis_t), intent(in) :: self
      type(string_t) diagnostics_string
    end function

  end interface

end module julienne_diagnosis_m
