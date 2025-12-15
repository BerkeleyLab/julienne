! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

! We normally only test the JULIENNE_PARALLEL_CALLBACKS feature 
! when HAVE_MULTI_IMAGE_SUPPORT is also enabled, but this test
! can also be force-enabled via -DTEST_PARALLEL_CALLBACKS
#if JULIENNE_PARALLEL_CALLBACKS && HAVE_MULTI_IMAGE_SUPPORT
#define TEST_PARALLEL_CALLBACKS 1
#endif

module multi_image_test_m
  !! Test JULIENNE_PARALLEL_CALLBACKS support
  use julienne_m, only : & 
     operator(.also.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
#if TEST_PARALLEL_CALLBACKS
    ,julienne_this_image &
    ,julienne_num_images &
    ,julienne_sync_all &
    ,julienne_co_sum_integer &
    ,julienne_error_stop &
#endif
    ,test_diagnosis_t &
    ,test_t &
    ,usher &
    ,test_description_t &
    ,test_result_t 
  implicit none

  private
  public :: multi_image_test_t, multi_image_setup

  type, extends(test_t) :: multi_image_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  logical, save :: setup_called = .false.
  integer, save :: this_image_cnt = 0 &
                  ,num_images_cnt = 0 &
                  ,sync_all_cnt = 0 &
                  ,co_sum_integer_cnt = 0

contains

  subroutine multi_image_setup()
# if TEST_PARALLEL_CALLBACKS
    julienne_this_image => julienne_callback_this_image
    julienne_num_images => julienne_callback_num_images
    julienne_sync_all => julienne_callback_sync_all
    julienne_co_sum_integer => julienne_callback_co_sum_integer
    julienne_error_stop => julienne_callback_error_stop
# endif
    setup_called = .true.
  end subroutine

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "JULIENNE_PARALLEL_CALLBACKS support"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(multi_image_test_t) multi_image_test

! some tests are conditional on JULIENNE_PARALLEL_CALLBACKS and otherwise skipped
#   if TEST_PARALLEL_CALLBACKS
#     define MAYBE(fn) , usher(fn)
#   else
#     define MAYBE(fn)
#   endif

    test_descriptions = [ &
       test_description_t("callback setup was performed", usher(check_julienne_callback_setup)) &
      ,test_description_t("callback pointers are still set" MAYBE(check_julienne_callback_ptrs)) &
      ,test_description_t("callback pointers are invoked as expected" MAYBE(check_julienne_callback_invocation)) &
    ]
    test_results = multi_image_test%run(test_descriptions)
  end function

  function check_julienne_callback_setup() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = setup_called
  end function

#if TEST_PARALLEL_CALLBACKS
  function check_julienne_callback_ptrs() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = .true.
    test_diagnosis = test_diagnosis .also. &
      associated(julienne_this_image, julienne_callback_this_image) .also. &
      associated(julienne_num_images, julienne_callback_num_images) .also. &
      associated(julienne_sync_all, julienne_callback_sync_all) .also. &
      associated(julienne_co_sum_integer, julienne_callback_co_sum_integer) .also. &
      associated(julienne_error_stop, julienne_callback_error_stop)
  end function

  function check_julienne_callback_invocation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    
    ! These callback properties check undocumented internal details
#   if HAVE_MULTI_IMAGE_SUPPORT
      test_diagnosis = &
               (this_image_cnt .isAtLeast. 1) &
        .also. (num_images_cnt .isAtLeast. 1) &
        .also. (co_sum_integer_cnt .isAtLeast. 1) &
#       if ASYNCHRONOUS_DIAGNOSTICS
          .also. (sync_all_cnt .equalsExpected. 0) 
#       else
          .also. (sync_all_cnt .isAtLeast. 1) 
#       endif
#   else
      test_diagnosis = &
               (this_image_cnt .equalsExpected. 0) &
        .also. (num_images_cnt .equalsExpected. 0) &
        .also. (co_sum_integer_cnt .equalsExpected. 0) &
        .also. (sync_all_cnt .equalsExpected. 0) 
#   endif
  end function

  ! --- callback functions --
  ! Currently just force trivial singleton runs
  function julienne_callback_this_image() result(this_image_id)
    implicit none
    integer :: this_image_id

    this_image_cnt = this_image_cnt + 1
#   if HAVE_MULTI_IMAGE_SUPPORT
      this_image_id = this_image()
#   else
      this_image_id = 1
#   endif
  end function

  function julienne_callback_num_images() result(image_count)
    implicit none
    integer :: image_count

    num_images_cnt = num_images_cnt + 1
#   if HAVE_MULTI_IMAGE_SUPPORT
      image_count = num_images()
#   else
      image_count = 1
#   endif
  end function

  subroutine julienne_callback_sync_all()
    implicit none

    sync_all_cnt = sync_all_cnt + 1
#   if HAVE_MULTI_IMAGE_SUPPORT
      sync all
#   endif
  end subroutine

  subroutine julienne_callback_co_sum_integer(a, result_image)
    implicit none
    integer, intent(inout), target :: a(:)
    integer, intent(in), optional :: result_image

    co_sum_integer_cnt = co_sum_integer_cnt + 1
#   if HAVE_MULTI_IMAGE_SUPPORT
      if (present(result_image)) then
        call co_sum(a, result_image)
      else
        call co_sum(a)
      end if
#   endif
  end subroutine

  subroutine julienne_callback_error_stop(stop_code_char)
    implicit none
    character(len=*), intent(in) :: stop_code_char

    error stop stop_code_char
  end subroutine

#endif

end module multi_image_test_m
