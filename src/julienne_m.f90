! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module julienne_m
  !! Global aggregation of all public entities
  use julienne_assert_m, only : call_julienne_assert_, julienne_assert
  use julienne_bin_m, only : bin_t
  use julienne_command_line_m, only : command_line_t
  use julienne_file_m, only : file_t
  use julienne_formats_m, only : separated_values, csv
  use julienne_github_ci_m, only : github_ci
  use julienne_one_image_prints_m, only : one_image_prints
  use julienne_string_m, only : string_t, array_of_strings &
    ,operator(.cat.) &
    ,operator(.csv.) &
    ,operator(.separatedBy.) & ! same as operator(.sv.)
    ,operator(.sv.)
  use julienne_test_description_m, only : test_description_t, filter
  use julienne_test_diagnosis_m, only   : test_diagnosis_t, diagnosis_function_i  &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.and.) &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.expect.) &
    ,operator(.isAfter.) &
    ,operator(.isAtLeast.) &
    ,operator(.isAtMost.) &
    ,operator(.isBefore.) &
    ,operator(.lessThan.) &
    ,operator(.lessThanOrEqualTo.) &    ! same as operator(.isAtMost.)
    ,operator(.greaterThan.) &
    ,operator(.greaterThanOrEqualTo.) & ! same as operator(.isAtLeast.)
    ,operator(.within.) &
    ,operator(.withinFraction.) &
    ,operator(.withinPercentage.)
  use julienne_test_fixture_m, only : test_fixture_t
  use julienne_test_harness_m, only : test_harness_t
  use julienne_test_result_m,  only : test_result_t
  use julienne_test_suite_m,   only : test_suite_t
  use julienne_test_m,         only : test_t

  implicit none
end module julienne_m
