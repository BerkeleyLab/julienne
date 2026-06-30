! Copyright (c), The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

submodule(julienne_stop_and_print_m) julienne_stop_and_print_s
  use julienne_string_m, only : operator(.csv.), operator(.separatedBy.)
  use julienne_file_m, only : file_t
  implicit none
  
contains 

  module procedure set_maxlen
    self%maxlen_ = length
  end procedure

  module procedure maxlen
    length = self%maxlen_
  end procedure

  module procedure stop_and_print

    select rank(data)
    rank(0)
      select type(data)
      type is(character(len=*))
        error stop data
      class is(string_t)
#ifndef __GFORTRAN__
        error stop data%string()
#else
        block
          character(len=:), allocatable :: stop_code
          stop_code = data%string()
          error stop stop_code
        end block
#endif
      class default
        call stop_and_print_header_data_footer(data, header, footer)
      end select
    rank default
      call stop_and_print_header_data_footer(data, header, footer)
    end select

  contains

    pure subroutine stop_and_print_header_data_footer(data, header, footer)
      character(len=*), intent(in), optional :: header, footer
      class(*), intent(in) :: data(..)
      character(len=:), allocatable :: code

      if (present(header)) then
        if (present(footer)) then
          code = new_line('') // header // new_line('') // character_stop_code(data) // new_line('') // footer // new_line('')
        else
          code = new_line('') // header // new_line('') // character_stop_code(data)
        end if
      else if (present(footer)) then
          code =                                           character_stop_code(data) // new_line('') // footer // new_line('')
      else
          code =                           new_line('') // character_stop_code(data) // new_line('')
      end if

      error stop code
    end subroutine

  end procedure

  module procedure character_stop_code

    type(string_t) stringy_stuff
    integer row, page

    select rank(stuff)
      rank(0)
        select type(stuff)
          type is(character(len=*))
            stringy_stuff = stuff
          type is(complex)
            stringy_stuff = string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(double precision)
            stringy_stuff = string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(file_t)
            stringy_stuff = stuff%lines() .separatedBy. new_line('')
            stop_code = stringy_stuff%string()
          type is(integer)
            stringy_stuff = string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(real)
            stringy_stuff = string_t(stuff)
            stop_code = stringy_stuff%string()
          class is(string_t)
            stop_code = stuff%string()
          class is(writable_t)
            allocate(character(len=stuff%maxlen()) :: stop_code)
            block
              integer io_status
              write(stop_code,*,iostat=io_status) stuff
              associate(code_maxlen => string_t(stuff%maxlen()))
                if (io_status /= 0) error stop "Call writable_t's set_maxlen procedure to increase stop_code maximum size above " // code_maxlen%string()
              end associate
            end block
            stop_code = trim(stop_code)
          class default
             error stop "character_stop_code (in print_and_stop_s): unsupported stop-code type for scalar"
        end select
      rank(1)
        select type(stuff)
          type is(character(len=*))
            stringy_stuff = .csv. string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(complex)
            stringy_stuff = .csv. string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(double precision)
            stringy_stuff = .csv. string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(integer)
            stringy_stuff = .csv. string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(real)
            stringy_stuff = .csv. string_t(stuff)
            stop_code = stringy_stuff%string()
          class is(string_t)
            stringy_stuff = .csv. stuff
            stop_code = stringy_stuff%string()
          class default
             error stop "character_stop_code (in print_and_stop_s): unsupported stop-code type for rank-1 array"
        end select
      rank(2)
        select type(stuff)
          type is(character(len=*))
            stringy_stuff =  [(.csv. string_t(stuff(row,:)) , row=1,size(stuff,1))] .separatedBy. new_line('')
            stop_code = stringy_stuff%string()
          type is(complex)
            stringy_stuff =  [(.csv. string_t(stuff(row,:)) , row=1,size(stuff,1))] .separatedBy. new_line('')
            stop_code = stringy_stuff%string()
          type is(double precision)
            stringy_stuff =  [(.csv. string_t(stuff(row,:)) , row=1,size(stuff,1))] .separatedBy. new_line('')
            stop_code = stringy_stuff%string()
          type is(integer)
            stringy_stuff =  [(.csv. string_t(stuff(row,:)) , row=1,size(stuff,1))] .separatedBy. new_line('')
            stop_code = stringy_stuff%string()
          type is(real)
            stringy_stuff =  [(.csv. string_t(stuff(row,:)) , row=1,size(stuff,1))] .separatedBy. new_line('')
            stop_code = stringy_stuff%string()
          class default
             error stop "character_stop_code (in print_and_stop_s): unsupported stop-code type for rank-2 array"
        end select
      rank(3)
        select type(stuff)
          type is(complex)
            stringy_stuff =  [( [(.csv. string_t(stuff(row,:,page)) , row=1,size(stuff,1))] .separatedBy. new_line(''), page = 1,size(stuff,3) )] .separatedBy. (new_line('') // new_line(''))
            stop_code = stringy_stuff%string()
          type is(double precision)
            stringy_stuff =  [( [(.csv. string_t(stuff(row,:,page)) , row=1,size(stuff,1))] .separatedBy. new_line(''), page = 1,size(stuff,3) )] .separatedBy. (new_line('') // new_line(''))
            stop_code = stringy_stuff%string()
          type is(integer)
            stringy_stuff =  [( [(.csv. string_t(stuff(row,:,page)) , row=1,size(stuff,1))] .separatedBy. new_line(''), page = 1,size(stuff,3) )] .separatedBy. (new_line('') // new_line(''))
            stop_code = stringy_stuff%string()
          type is(real)
            stringy_stuff =  [( [(.csv. string_t(stuff(row,:,page)) , row=1,size(stuff,1))] .separatedBy. new_line(''), page = 1,size(stuff,3) )] .separatedBy. (new_line('') // new_line(''))
            stop_code = stringy_stuff%string()
          class default
             error stop "character_stop_code (in print_and_stop_s): unsupported stop-code type for rank-3 array"
        end select
      rank default
        associate(stop_code_rank => string_t(stop_code))
          error stop "character_stop_code (in print_and_stop_s): unsupported stop-code rank: " //  stop_code_rank%string()
        end associate
    end select
  end procedure
  
end submodule julienne_stop_and_print_s
