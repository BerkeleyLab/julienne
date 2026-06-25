! Copyright (c), The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

module julienne_stop_and_print_m
  !! Define a pure subroutine subroutine that prints string_t objects/arrays
  use julienne_string_m, only : string_t
  implicit none
  
  private
  public :: stop_and_print
  
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
    error stop new_line('') // header // new_line('') // stringify(data)
  end subroutine

  pure function stringify(stuff) result(characters)
    class(*), intent(in) :: stuff(..)
    character(len=:), allocatable :: characters

    type(string_t) stringy_stuff
    integer row

    select rank(stuff)
      rank(0)
        select type(stuff)
          type is(character(len=*))
            stringy_stuff = stuff
          type is(complex)
            stringy_stuff = string_t(stuff)
            characters = stringy_stuff%string()
          type is(double precision)
            stringy_stuff = string_t(stuff)
            characters = stringy_stuff%string()
          type is(integer)
            stringy_stuff = string_t(stuff)
            characters = stringy_stuff%string()
          type is(real)
            stringy_stuff = string_t(stuff)
            characters = stringy_stuff%string()
          class default
             error stop "stringify: unsupported type"
        end select
      rank(1)
        select type(stuff)
          type is(character(len=*))
            stringy_stuff = .csv. string_t(stuff)
            characters = stringy_stuff%string()
          type is(complex)
            stringy_stuff = .csv. string_t(stuff)
            characters = stringy_stuff%string()
          type is(double precision)
            stringy_stuff = .csv. string_t(stuff)
            characters = stringy_stuff%string()
          type is(integer)
            stringy_stuff = .csv. string_t(stuff)
            characters = stringy_stuff%string()
          type is(real)
            stringy_stuff = .csv. string_t(stuff)
            characters = stringy_stuff%string()
          class default
             error stop "stringify: unsupported type"
        end select
      rank(2)
        select type(stuff)
          type is(character(len=*))
            stringy_stuff = .csv. [(.csv. string_t(stuff(row,:)) // new_line(''), row=1,size(stuff,2))]
            characters = stringy_stuff%string()
          type is(complex)
            stringy_stuff = .csv. [(.csv. string_t(stuff(row,:)) // new_line(''), row=1,size(stuff,2))]
            characters = stringy_stuff%string()
          type is(double precision)
            stringy_stuff = .csv. [(.csv. string_t(stuff(row,:)) // new_line(''), row=1,size(stuff,2))]
            characters = stringy_stuff%string()
          type is(integer)
            stringy_stuff = .csv. [(.csv. string_t(stuff(row,:)) // new_line(''), row=1,size(stuff,2))]
            characters = stringy_stuff%string()
          type is(real)
            stringy_stuff = .csv. [(.csv. string_t(stuff(row,:)) // new_line(''), row=1,size(stuff,2))]
            characters = stringy_stuff%string()
          class default
             error stop "stringify: unsupported type"
        end select
      rank default
        error stop "stringify: unsupported rank"
    end select
  end function
  
end module
