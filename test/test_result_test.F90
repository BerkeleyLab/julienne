! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90" 

module test_result_test_m
  !! Verify test_result_t object behavior
  use julienne_m, only : string_t, test_result_t, test_description_t, test_t, test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: test_result_test_t

  type, extends(test_t) :: test_result_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_result_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), check_array_result_construction), &
      test_description_t(string_t("reporting failure if the test fails on one image"), check_single_image_failure) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_array_ptr, check_single_ptr
    check_array_ptr => check_array_result_construction
    check_single_ptr => check_single_image_failure
    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), check_array_ptr), &
      test_description_t(string_t("reporting failure if the test fails on one image"), check_single_ptr) &
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function check_array_result_construction() result(passed)
    type(test_result_t), allocatable :: test_results(:)
    logical passed

    test_results = test_result_t(["foo","bar"], [.true.,.false.])
    passed = size(test_results)==2
  end function

  function check_single_image_failure() result(passed)
    type(test_result_t), allocatable :: test_result
    logical passed

#if HAVE_MULTI_IMAGE_SUPPORT
    if (this_image()==1) then
#endif

      test_result = test_result_t("image 1 fails", .false.)

#if HAVE_MULTI_IMAGE_SUPPORT
    else
      test_result = test_result_t("all images other than 1 pass", .true.)
    end if
#endif

    passed = .not. test_result%passed()
  end function

end module test_result_test_m
