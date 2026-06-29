! Copyright (c), The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_stop_and_print_m
  !! Define a pure subroutine subroutine that prints string_t objects/arrays
  use julienne_string_m, only : string_t, operator(.csv.), operator(.separatedBy.)
  implicit none
  
  private
  public :: stop_and_print
  public :: character_stop_code
  
  interface stop_and_print
    module procedure print_string
    module procedure print_header_and_data
  end interface

contains 

  pure subroutine print_string(message)
    implicit none
    type(string_t), intent(in) :: message
    error stop message%string()
  end subroutine

  pure subroutine print_header_and_data(header, data)
    implicit none
    character(len=*), intent(in) :: header
    class(*), intent(in) :: data
#ifndef __GFORTRAN__
    error stop new_line('') // header // new_line('') // character_stop_code(data)
#else
    block
      character(len=:), allocatable :: code
      code = new_line('') // header // new_line('') // character_stop_code(data)
      error stop code
    end block
#endif
  end subroutine

  pure function character_stop_code(stuff) result(stop_code)
    class(*), intent(in) :: stuff(..)
    character(len=:), allocatable :: stop_code

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
          type is(integer)
            stringy_stuff = string_t(stuff)
            stop_code = stringy_stuff%string()
          type is(real)
            stringy_stuff = string_t(stuff)
            stop_code = stringy_stuff%string()
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
  end function
  
end module
