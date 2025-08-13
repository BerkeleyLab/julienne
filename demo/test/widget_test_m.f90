! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module widget_test_m
  use julienne_m, only : test_t, test_description_t, test_diagnosis_t, test_result_t
  use julienne_m, only : operator(.approximates.), operator(.within.)
  use widget_m, only : widget_t
  implicit none

  type, extends(test_t) :: widget_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'A widget'
  end function

  function results() result(test_results)
    type(widget_test_t) widget_test
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    test_descriptions = [ &
       test_description_t('checking something', check_something) &
      ,test_description_t('doing something', do_something) &
    ]
    test_results = widget_test%run(test_descriptions)
  end function

  function check_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = 1. .approximates. 2. .within. 3.
  end function

  function do_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = test_diagnosis_t(test_passed = 0 == 1, diagnostics_string = 'impossible result')
  end function

end module
