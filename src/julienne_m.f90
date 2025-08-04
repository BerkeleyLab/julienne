! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_m
  !! Global aggregation of all public entities
  use julienne_bin_m, only : bin_t
  use julienne_command_line_m, only : command_line_t
  use julienne_file_m, only : file_t
  use julienne_formats_m, only : separated_values, csv
  use julienne_github_ci_m, only : github_ci
  use julienne_test_description_m, only : test_description_t, diagnosis_function_i
  use julienne_test_diagnosis_m, only : &
     test_diagnosis_t &
    ,call_julienne_assert_ &
    ,julienne_assert &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.and.) & ! ssame as .also. TODO: deprecate .and. to avoid confusion with intrinsic operator
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,operator(.isAtMost.) &
    ,operator(.isBefore.) &
    ,operator(.isAfter.) &
    ,operator(.lessThan.) &
    ,operator(.lessThanOrEqualTo.) &    ! same as operator(.isAtMost.)
    ,operator(.greaterThan.) &
    ,operator(.greaterThanOrEqualTo.) & ! same as operator(.isAtLeast.)
    ,operator(.within.) &
    ,operator(.withinFraction.) &
    ,operator(.withinPercentage.)
  use julienne_string_m, only : &
     string_t &
    ,array_of_strings &
    ,operator(.cat.) &
    ,operator(.csv.) &
    ,operator(.separatedBy.) & ! same as operator(.sv.)
    ,operator(.sv.)
  use julienne_test_m, only : test_t, test_description_substring
  use julienne_test_result_m, only : test_result_t

  !! Deprecated features:
  use julienne_vector_test_description_m, only : vector_test_description_t, vector_diagnosis_function_i
  implicit none
end module julienne_m
