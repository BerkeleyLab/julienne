program legacy_unit_test_failure_test
  !! Conditionally execute test_test_t%report

#ifdef __GNUC__
#  if (GCC_VERSION < 140300)

  ! Internal utilities
  use julienne_m, only : command_line_t, GitHub_CI, string_t
  use test_test_m, only : test_test_t
  implicit none

#    if TEST_INTENTIONAL_FAILURE
       type(test_test_t) test_test
       type(command_line_t) command_line
       integer :: passes=0, tests=0, skips=0

       associate(command_line => command_line_t(), me => this_image())
        if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
          call test_test%report(passes, tests, skips)
          if (passes==0) then
            print '(a)', "Test(s) intended to fail failed as intended."
          else
            associate(passes_ => string_t(passes))
              error stop passes_%string() // "test(s) intended to fail unintendedly passed."
            end associate
          end if
        end if
       end associate

#    else

       associate(command_line => command_line_t(), me => this_image())
         if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
           if (me==1) then
             print '(a)', new_line('') // &
                          'Skipping the test in ' // __FILE__ // '.' // new_line('') // &
                          'Add the following to your fpm command to test unit-test failure: --flag "-DTEST_INTENTIONAL_FAILURE"' //&
                          new_line('')
           end if
         end if
       end associate

#    endif
#  endif
#endif

end program
