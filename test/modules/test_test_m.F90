! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module test_test_m
  !! Conditionally test that failure of a test on only one image is reported as a test failure
  use julienne_m, only : &
     operator(.expect.) &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: test_test_t

  type, extends(test_t) :: test_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_t type"
  end function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_test_t) test_test

    test_results = test_test%run([ &
      test_description_t("reporting failure if a single image fails a test", check_one_image_fails) &
    ])
  end function

#else

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_test_t) test_test
    procedure(diagnosis_function_i), pointer :: &
      check_one_image_fails_ptr => check_one_image_fails

    test_results = test_test%run([ &
      test_description_t("reporting failure if a single image fails a test", check_one_image_fails_ptr) &
    ])
  end function

#endif

  function check_one_image_fails() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image(), images => num_images())
#else
    associate(me => 1, images => 1)
#endif
      test_diagnosis = .expect. (me == images)
    end associate
  end function

end module test_test_m
