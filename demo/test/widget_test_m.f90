! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module widget_test_m
  use julienne_m, only : test_t, test_description_t, test_diagnosis_t, test_result_t
  use julienne_m, only : operator(.approximates.), operator(.within.), operator(.all.)
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
    test_results = widget_test%run( & 
      [test_description_t('doing something', do_something) &
      ,test_description_t('checking something', check_something) &
      ,test_description_t('skipping something') &
    ])
  end function

  function check_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(widget_t) widget
    test_diagnosis = .all.([22./7., 3.14159] .approximates. widget%pi() .within. 0.001)
  end function

  function do_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = test_diagnosis_t(test_passed = 1 == 1, diagnostics_string = 'craziness ensued')
  end function

end module
