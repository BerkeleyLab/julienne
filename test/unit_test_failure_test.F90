program unit_test_failure_test
  !! Conditionally execute test_test_t%report

#if (! defined(__GNUC__)) || (GCC_VERSION >= 140300)

  use julienne_m, only : command_line_t, test_fixture_t, test_harness_t
  use test_test_m, only : test_test_t
  implicit none

#  if TEST_INTENTIONAL_FAILURE
    associate(command_line => command_line_t())
      if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
        associate(test_harness => test_harness_t([test_fixture_t(test_test_t())]))
          call test_harness%report_results
        end associate
      end if
    end associate
#  else
#    if HAVE_MULTI_IMAGE_SUPPORT
       associate(command_line => command_line_t(), me => this_image())
#    else
       associate(command_line => command_line_t(), me => 1)
#    endif
         if (.not. command_line%argument_present([character(len=len("--help"))::"--help","-h"])) then
           if (me==1) print '(a)', &
             new_line('') // &
             'Skipping the test in ' // __FILE__ // '.' // new_line('') // &
             'Add the following to your fpm command to test unit-test failures: --flag "-DTEST_INTENTIONAL_FAILURE"' // &
             new_line('')
         end if
       end associate
#  endif
#endif

end program
