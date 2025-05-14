module assertion_m
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  use julienne_string_m, only : string_t
  implicit none

  private
  public :: assertion_t

  type, extends(test_diagnosis_t) :: assertion_t
  end type

  interface assertion_t

    pure module function construct_from_parent(test_diagnosis) result(assertion)
      implicit none
      type(test_diagnosis_t), intent(in) :: test_diagnosis
      type(assertion_t) assertion
    end function

    elemental module function construct_from_string_t(assertion_succeeds, diagnostics_string) result(assertion)
      !! The result is an assertion_t object with a parent test_diagnosis_t defined by the dummy arguments
      implicit none
      logical, intent(in) :: assertion_succeeds
      type(string_t), intent(in) :: diagnostics_string
      type(assertion_t) assertion
    end function

    elemental module function construct_from_character(assertion_succeeds, diagnostics_string) result(assertion)
      !! The result is a assertion_t object with a parent test_diagnosis_t defined by the dummy arguments
      implicit none
      logical, intent(in) :: assertion_succeeds
      character(len=*), intent(in) :: diagnostics_string
      type(assertion_t) assertion
    end function

  end interface

end module
