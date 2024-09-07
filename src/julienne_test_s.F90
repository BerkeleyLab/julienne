! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_test_m) julienne_test_s
  use julienne_user_defined_collectives_m, only : co_all
  use julienne_command_line_m, only : command_line_t
  implicit none

#if defined(__flang__)
  #define NO_MULTI_IMAGE_SUPPORT
#endif

contains

  module procedure report
#ifdef NO_MULTI_IMAGE_SUPPORT
    integer me
    me = 1
#else
    associate(me => this_image())
#endif


      if (me==1) then

        first_report: &
        if (.not. allocated(test_description_substring)) then
          block 
            type(command_line_t) command_line
            test_description_substring = command_line%flag_value("--contains")
          end block
          if (len(test_description_substring)==0) then
            print*,"Running all tests."
            print*,"(Add '-- --contains <string>' to run only tests with subjects or descriptions containing the specified string.)"
          else
            print *,"Running only tests with subjects or descriptions containing '", test_description_substring,"'."
          end if
        end if first_report

        print *, new_line('a'), test%subject()

      end if

#ifndef NO_MULTI_IMAGE_SUPPORT
      call co_broadcast(test_description_substring, source_image=1)
#endif
      
#ifndef _CRAYFTN
      associate(test_results => test%results())
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            block
              integer i
              do i=1,num_tests
                if (me==1) print *,"   ",test_results(i)%characterize()
              end do
            end block
          end if
          block 
            logical, allocatable :: passing_tests(:)
            passing_tests = test_results%passed()
#ifndef NO_MULTI_IMAGE_SUPPORT
            call co_all(passing_tests)
#endif
            associate(num_passes => count(passing_tests))
              if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
              passes = passes + num_passes
            end associate
          end block
        end associate
#ifndef NO_MULTI_IMAGE_SUPPORT
      end associate
#endif

#else
      block
        logical, allocatable :: passing_tests(:)
        type(test_result_t), allocatable :: test_results(:)
        integer i

        test_results = test%results()
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            do i=1,num_tests
              if (me==1) print *,"   ",test_results(i)%characterize()
            end do
          end if
          passing_tests = test_results%passed()
          call co_all(passing_tests)
          associate(num_passes => count(passing_tests))
            if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
            passes = passes + num_passes
          end associate
        end associate
      end block
#endif

    end associate

  end procedure

end submodule julienne_test_s
