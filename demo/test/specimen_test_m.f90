! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module specimen_test_m
  use julienne_m, only : &
     test_t, test_description_t, test_diagnosis_t, test_result_t &
    ,operator(.approximates.), operator(.within.), operator(.all.), operator(//)
  use specimen_m, only : specimen_t
  implicit none

  type, extends(test_t) :: specimen_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'A specimen'
  end function

  function results() result(test_results)
    type(specimen_test_t) specimen_test
    type(test_result_t), allocatable :: test_results(:)
    test_results = specimen_test%run( & 
      [test_description_t('doing something', do_something) &
      ,test_description_t('checking something', check_something) &
      ,test_description_t('skipping something') &
    ])
  end function

  function check_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(specimen_t) specimen
    test_diagnosis = .all.( &
       [22./7., 3.14159] .approximates. specimen%pi() .within. 0.001 &
    ) // ' (pi approximation)'
  end function

  function do_something() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = &
      test_diagnosis_t(test_passed = 1 == 1, diagnostics_string = 'craziness ensued')
  end function

end module
