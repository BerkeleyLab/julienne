! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program false_assertion_diagnosis
  !! Test an assertion that is hardwired to fail by directly calling julienne_assert
  !! with an assertion_diagnosis_t actual argument constructed with success=.false.

#ifdef RUN_FALSE_ASSERTIONS

  use julienne_m, only : assert_diagnosis, assertion_diagnosis_t
  implicit none

  call assert_diagnosis(assertion_diagnosis_t(success=.false., diagnostics_string="(intentionally false assertion diagnosis)"))

#endif

end program

