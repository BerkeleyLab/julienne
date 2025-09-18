! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module user_defined_collectives_test_m
  !! Verify collectives_t object behavior
  use julienne_m, only : &
     co_all &
    ,co_gather &
    ,operator(.all.) &
    ,operator(.csv.) &
    ,operator(.expect.) &
    ,operator(.equalsExpected.) &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: user_defined_collectives_test_t

  type, extends(test_t) :: user_defined_collectives_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The user-defined collective procedures"
  end function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(user_defined_collectives_test_t) user_defined_collectives_test

    test_results = user_defined_collectives_test%run([ &
      test_description_t("co_gather gathering distributed strings into one array", check_gather_characters) &
     ,test_description_t("co_all returning .false. if the argument is .false. on only one image ", check_co_all) &
    ])
  end function
#else
  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(user_defined_collectives_test_t) user_defined_collectives_test
    procedure(diagnosis_function_i), pointer :: &
       check_gather_characters_ptr => check_gather_characters &
      ,check_co_all => check_co_all

    test_results = user_defined_collectives_test%run([ &
      test_description_t("co_gather gathering distributed strings into one array", check_gather_characters_ptr) &
     ,test_description_t("co_all returning .false. if the argument is .false. on only one image ", check_co_all) &
    ])
  end function
#endif

  function check_gather_characters() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: strings(*) = [character(len=len("234567")) :: "1", "234567", "890"]
    integer s

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image(), images => num_images())
#else
    associate(me => 1, images => 1)
#endif
      associate(num_strings => size(strings))
        test_diagnosis = .all. (co_gather(strings(mine(me, num_strings))) .equalsExpected. [(strings(mine(s, num_strings)), s = 1, images)])
      end associate
    end associate
  contains
    pure function mine(image, num_elements) result(element)
      integer, intent(in):: image, num_elements
      integer element
      element = 1 + mod(image-1, num_elements)
    end function
  end function

  function check_co_all() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    logical boolean

#if HAVE_MULTI_IMAGE_SUPPORT
    associate(me => this_image(), images => num_images())
#else
    associate(me => 1, images => 1)
#endif
      boolean = merge(.false., .true., me==images) 
      call co_all(boolean)
      test_diagnosis = .expect. (.not. boolean)
    end associate
  end function

end module user_defined_collectives_test_m
