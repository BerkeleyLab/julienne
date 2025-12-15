! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
#include "assert_macros.h"

submodule(julienne_multi_image_m) julienne_multi_image_s
  use assert_m
  implicit none

contains

  module procedure internal_this_image
#   if !HAVE_MULTI_IMAGE_SUPPORT
      this_image_id = 1
#   elif JULIENNE_PARALLEL_CALLBACKS
      if (associated(julienne_this_image)) then
        this_image_id = julienne_this_image()
      else
        this_image_id = 1 ! callback unset, assume single-image
        call_assert(internal_num_images() == 1)
      end if
#   else 
      this_image_id = this_image()
#   endif
  end procedure

  module procedure internal_num_images
#   if !HAVE_MULTI_IMAGE_SUPPORT
      image_count = 1
#   elif JULIENNE_PARALLEL_CALLBACKS
      if (associated(julienne_num_images)) then
        image_count = julienne_num_images()
      else
        image_count = 1 ! callback unset, assume single-image
      end if
#   else 
      image_count = num_images()
#   endif
  end procedure

  module procedure internal_sync_all
#   if !HAVE_MULTI_IMAGE_SUPPORT
      ; ! nothing to do
#   elif JULIENNE_PARALLEL_CALLBACKS
      if (associated(julienne_sync_all)) then
        call julienne_sync_all()
      else
        ; ! assume single-image, no-op
        call_assert(internal_num_images() == 1)
      end if
#   else 
      sync all
#   endif
  end procedure

  module procedure internal_co_sum_integer
#   if !HAVE_MULTI_IMAGE_SUPPORT
      ; ! nothing to do
#   elif JULIENNE_PARALLEL_CALLBACKS
      if (associated(julienne_co_sum_integer)) then
        call julienne_co_sum_integer(a, result_image)
      else
        ; ! assume single-image, no-op
        call_assert(internal_num_images() == 1)
      end if
#   else 
      ! this branch is a bug workaround for ifx 2025.2
      if (present(result_image)) then 
        call co_sum(a, result_image)
      else
        call co_sum(a)
      end if
#   endif
  end procedure

  module procedure internal_error_stop
#   if JULIENNE_PARALLEL_CALLBACKS
      if (associated(julienne_error_stop)) then
        call julienne_error_stop(stop_code_char)
      else
        ; ! deliberate fall-thru
      end if
#   endif
    error stop stop_code_char
  end procedure

end submodule julienne_multi_image_s
