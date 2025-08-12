! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module bin_test_m
  !! Check data partitioning across bins
  use julienne_m, only : &
     bin_t &
    ,filter &
    ,operator(.csv.) &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  use assert_m, only : assert
  implicit none

  private
  public :: bin_test_t

  type, extends(test_t) :: bin_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "An array of bin_t objects (bins)"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t(string_t("partitioning items nearly evenly across bins"), check_block_partitioning), &
      test_description_t(string_t("partitioning all item across all bins without item loss"), check_all_items_partitioned) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: check_block_partitioning_ptr, check_all_items_ptr
    check_block_partitioning_ptr => check_block_partitioning
    check_all_items_ptr => check_all_items_partitioned
    test_descriptions = [ &
      test_description_t(string_t("partitioning items nearly evenly across bins"), check_block_partitioning_ptr), &
      test_description_t(string_t("partitioning all item across all bins without item loss"), check_all_items_ptr) &
    ]
#endif
    associate(matching_descriptions => filter(test_descriptions, subject()))
       test_results = matching_descriptions%run()
    end associate
  end function

  function check_block_partitioning() result(test_diagnosis)
    !! Check that the items are partitioned across bins evenly to within a difference of one item per bin
    type(test_diagnosis_t) test_diagnosis

    type(bin_t), allocatable :: bins(:)
    integer, parameter :: n_items=11, n_bins=7
    integer b

    bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]

    associate(in_bin => [(bins(b)%last() - bins(b)%first() + 1, b = 1, n_bins)])
      associate(remainder => mod(n_items, n_bins), items_per_bin => n_items/n_bins)
        associate(expected_distribution => [ [(items_per_bin+1, b=1,remainder)], [(items_per_bin, b=remainder+1,n_bins)] ])
          test_diagnosis = test_diagnosis_t( &
             test_passed = all(in_bin == expected_distribution) &
            ,diagnostics_string = "expected " // .csv. string_t(expected_distribution) // "; actual " // .csv. string_t(in_bin) &
          )
        end associate
      end associate
    end associate

  end function

  function check_all_items_partitioned() result(test_diagnosis)
    !! Check that the number of items in each bin sums to the total number of items
    type(test_diagnosis_t) test_diagnosis

    type(bin_t), allocatable :: bins(:)
    integer, parameter :: n_items=11, n_bins=6
    integer b

    bins = [( bin_t(num_items=n_items, num_bins=n_bins, bin_number=b), b = 1,n_bins )]

    associate(items_in_bins => sum([(bins(b)%last() - bins(b)%first() + 1, b = 1, n_bins)]))
      test_diagnosis = test_diagnosis_t( &
         test_passed = items_in_bins == n_items &
        ,diagnostics_string = "expected " // string_t(n_items) // ", actual " // string_t(items_in_bins) &
      )
    end associate
  end function

end module bin_test_m
