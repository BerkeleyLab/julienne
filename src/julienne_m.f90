! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module julienne_m
  !! Global aggregation of all public entities
  use julienne_bin_m, only : bin_t
  use julienne_command_line_m, only : command_line_t
  use julienne_file_m, only : file_t
  use julienne_github_ci_m, only : github_ci
  use julienne_formats_m, only : separated_values, csv
  use julienne_string_m, only : string_t, operator(.cat.)
  use julienne_test_m, only : test_t, test_description_substring
  use julienne_test_description_m, only : test_description_t, test_function_i
  use julienne_test_result_m, only : test_result_t
  use julienne_vector_test_description_m, only : vector_test_description_t, vector_function_strategy_t

  implicit none
  private
  public :: bin_t
  public :: csv
  public :: command_line_t
  public :: operator(.cat.)
  public :: file_t
  public :: github_ci
  public :: separated_values
  public :: string_t
  public :: test_t
  public :: test_description_t
  public :: test_description_substring
  public :: test_function_i
  public :: test_result_t
  public :: vector_function_strategy_t
  public :: vector_test_description_t
end module julienne_m
