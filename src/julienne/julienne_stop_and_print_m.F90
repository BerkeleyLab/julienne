! Copyright (c), The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_stop_and_print_m
  !! Define a pure subroutine subroutine that prints string_t objects/arrays
  use julienne_string_m, only : string_t, operator(.csv.), operator(.separatedBy.)
  use julienne_file_m, only : file_t
  implicit none
  
  private
  public :: stop_and_print
  public :: character_stop_code
  public :: writable_t
  
  interface stop_and_print
    module procedure print_string
    module procedure print_header_and_data
  end interface

  type, abstract :: writable_t
    private
    integer :: maxlen_ = 16384
  contains
    generic :: write(formatted) => write_formatted
    procedure(write_formatted_i), deferred :: write_formatted
    procedure :: set_maxlen
    procedure :: maxlen
  end type

  abstract interface

    subroutine write_formatted_i(self, unit, edit_descriptor, v_list, iostat, iomsg)
      import writable_t
      class(writable_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: edit_descriptor
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

  end interface

contains 

  pure subroutine print_string(message)
    type(string_t), intent(in) :: message
    error stop message%string()
  end subroutine

  pure subroutine set_maxlen(self, length)
    class(writable_t), intent(inout) :: self
    integer, intent(in) :: length
    self%maxlen_ = length
  end subroutine

  pure function maxlen(self) result(length)
    class(writable_t), intent(in) :: self
    integer length
    length = self%maxlen_
  end function

  pure subroutine print_header_and_data(header, data)
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
  end function
  
end module
