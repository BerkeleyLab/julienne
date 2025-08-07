! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_assert_m
  !! Define interfaces for writing assertions
  use julienne_test_diagnosis_m, only : test_diagnosis_t
  implicit none

  private
  public :: call_julienne_assert_
  public :: julienne_assert

  interface call_julienne_assert_

    pure module subroutine julienne_assert(test_diagnosis, file, line)
      !! This subroutine wraps the Assert library's `assert` subroutine.
      !!
      !! Use Cases
      !! ---------
      !!   1. Invoke julienne_assert via the generic interface `call_julienne_assert` to
      !!      facilitate complete removal when compiling without the flag `-DASSERTIONS`.
      !!   2. Invoke julienne_assert via direct procedure call to guarantee execution.
      !!
      !! Usage
      !! -----
      !! Make the only actual argument an expression containing `test_diagnosis_t` defined
      !! operations, such as `x .approximates. y .within. tolerance`.  The expression
      !! result will be a `test_diagnosis_t` object on which `julienne_assert` will invoke
      !! the `diagnostics_string()` type-bound procedure, the result of which julienne_assert
      !! will include in the stop code of an `error stop` if the expresssion is untrue.
      !! The resulting stop code will contain such information as the operand values and
      !! roles (expected value, actual value, tolerance value).  In use case 1, compiling
      !! with `-DASSERTIONS` will cause the preprocessor to insert the corresponding
      !! invocations's line number and the encompassing file's name as the `file` and `line`
      !! arguments, respectively, which `julienne_assert` will include in the stop code.
      !! Most compilers will write the stop code to `error_unit`.
      !!
      !! If a literal reproduction of the test expression suffices, such as when the
      !! expression is `allocated(a)`, then instead invoke the Assert library's `assert`
      !! subroutine by that library's `call_assert` macro or by direct call.
      !! When invoking via the macro, make the only actual argument, `assertion`, a
      !! `logical` expression.  Then if compiling with `-DASSERTIONS` and if the assertion
      !! evaluates to `.false.`, the stop code will include the text of the expression
      !! argument, the file name, and the line number of the `call_assert` macro invocation.
      implicit none
      type(test_diagnosis_t), intent(in) :: test_diagnosis
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
    end subroutine

  end interface

end module julienne_assert_m
