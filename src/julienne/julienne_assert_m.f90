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
      !! Error terminate if `test_diagnosis%test_passed() == .false.`, in which
      !! case the stop code contains
      !!
      !!   1. The description argument if present and if called via 
      !!      `julienne_assert; otherwise, a copy of the invoking statement,
      !!   2. The value of `test_diagnosis%diagnostics_string(),`,
      !!   3. The file name if present, and 
      !!   4. The line number if present.
      !!
      !! Most compilers write the stop code to `error_unit`.
      !!
      !! Usage
      !! -----
      !!
      !!   `call julienne_assert(.all. (["a","b","c"] .isBefore. "efg"))`
      !!   `call_julienne_assert(.all. (["a","b","c"] .isBefore. "efg"))`
      !!
      !! The first line above guarantees execution, whereas the second ensures
      !! removal when compiled without `-DASSERTIONS`.  When invoked via macro,
      !! the second line also causes the automatic insertion of items 1-4 above.
      implicit none
      type(test_diagnosis_t), intent(in) :: test_diagnosis
      character(len=*), intent(in), optional :: file
      integer, intent(in), optional :: line
    end subroutine

  end interface

end module julienne_assert_m
