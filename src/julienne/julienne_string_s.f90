! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_string_m) julienne_string_s
  use assert_m, only : assert, intrinsic_array_t
  implicit none

  integer, parameter :: integer_width_supremum = 11, default_real_width_supremum = 18, double_precision_width_supremum = 25
  integer, parameter :: logical_width=2, comma_width = 1, parenthesis_width = 1, space=1
  
contains

  module procedure as_character
    raw_string = self%string_
  end procedure

  module procedure is_allocated
    string_allocated = allocated(self%string_)
  end procedure

  module procedure from_characters
    new_string%string_ = string
  end procedure

  module procedure from_default_integer
    allocate(character(len=integer_width_supremum) :: string%string_)
    write(string%string_, '(g0)') i
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_default_real
    allocate(character(len=double_precision_width_supremum) :: string%string_)
    write(string%string_, '(g0)') x
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_double_precision
    allocate(character(len=double_precision_width_supremum) :: string%string_)
    write(string%string_, '(g0)') x
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_default_logical
    allocate(character(len=logical_width) :: string%string_)
    write(string%string_, '(g0)') b
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_logical_c_bool
    allocate(character(len=logical_width) :: string%string_)
    write(string%string_, '(g0)') b
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_default_complex
    allocate(character(len=2*default_real_width_supremum + 2*parenthesis_width + comma_width) :: string%string_)
    write(string%string_, *) z
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure from_double_precision_complex
    allocate(character(len=space + 2*double_precision_width_supremum + 2*parenthesis_width + comma_width) :: string%string_)
    write(string%string_, *) z
    string%string_ = trim(adjustl(string%string_))
  end procedure

  module procedure concatenate_elements
    integer s 

    concatenated_strings = ""
    do s = 1, size(strings)
      concatenated_strings = concatenated_strings // strings(s)%string()
    end do
  end procedure

  module procedure strings_with_comma_separator
    csv = strings_with_string_t_separator(strings, string_t(","))
  end procedure 

  module procedure characters_with_comma_separator
    csv = strings_with_string_t_separator(string_t(strings), string_t(","))
  end procedure 

  module procedure characters_with_character_separator
    sv = strings_with_string_t_separator(string_t(strings), string_t(separator))
  end procedure 

  module procedure characters_with_string_separator
    sv = strings_with_string_t_separator(string_t(strings), separator)
  end procedure 

  module procedure strings_with_character_separator
    sv = strings_with_string_t_separator(strings, string_t(separator))
  end procedure 

  module procedure strings_with_string_t_separator

    integer s 

    associate(num_elements => size(strings))

      sv = ""

      do s = 1, num_elements - 1
        sv = sv // strings(s) // separator
      end do

      sv = sv // strings(num_elements)

    end associate

  end procedure

  module procedure array_of_strings
    character(len=:), allocatable :: remainder, next_string
    integer next_delimiter, string_end

    remainder = trim(adjustl(delimited_strings))
    allocate(strings_array(0))

    do  
      next_delimiter = index(remainder, delimiter)
      string_end = merge(len(remainder), next_delimiter-1, next_delimiter==0)
      next_string = trim(adjustl(remainder(:string_end)))
      if (len(next_string)==0) exit
      strings_array = [strings_array, string_t(next_string)]
      if (next_delimiter==0) then
        remainder = ""
      else
        remainder = trim(adjustl(remainder(next_delimiter+1:)))
      end if
    end do

  end procedure

  module procedure get_json_key
    character(len=:), allocatable :: raw_line
  
    raw_line = self%string()
    associate(opening_key_quotes => index(raw_line, '"'), separator => index(raw_line, ':'))
      associate(closing_key_quotes => opening_key_quotes + index(raw_line(opening_key_quotes+1:), '"'))
        unquoted_key = string_t(trim(raw_line(opening_key_quotes+1:closing_key_quotes-1)))
      end associate
    end associate

  end procedure

  module procedure file_extension
    character(len=:), allocatable :: name_

    name_ = trim(adjustl(self%string()))

    associate( dot_location => index(name_, '.', back=.true.) )
      if (dot_location < len(name_)) then
        extension = trim(adjustl(name_(dot_location+1:)))
      else
        extension = ""
      end if
    end associate
  end procedure

  module procedure base_name
    character(len=:), allocatable :: name_

    name_ = self%string()
    
    associate(dot_location => index(name_, '.', back=.true.) )
      if (dot_location < len(name_)) then
        base = trim(adjustl(name_(1:dot_location-1)))
      else
        base = ""
      end if
    end associate
  end procedure

  module procedure get_real_with_character_key
    value_ = self%get_real(string_t(key), mold)
  end procedure

  module procedure get_double_precision_with_character_key
    value_ = self%get_double_precision(string_t(key), mold)
  end procedure

  module procedure get_real
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_real): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        read(string_value, fmt=*) value_
      end associate
    end associate

  end procedure

  module procedure get_double_precision
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_double_precision): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        read(string_value, fmt=*) value_
      end associate
    end associate

  end procedure

  module procedure get_character
    associate(string_value => self%get_string(key, string_t(mold)))
      value_ = string_value%string()
    end associate
  end procedure

  module procedure get_character_with_character_key
    associate(string_value => self%get_string(string_t(key), string_t(mold)))
      value_ = string_value%string()
    end associate
  end procedure

  module procedure get_string_t_array_with_string_t_key
    value_ = self%get_string_t_array_with_character_key(key%string(), mold)
  end procedure

  module procedure get_string_t_array_with_character_key

    character(len=:), allocatable :: raw_line
    integer i, comma, opening_quotes, closing_quotes

    call assert(key==self%get_json_key(), "key==self%get_string_json()", key)

    raw_line = self%string()

    associate(colon => index(raw_line, ':'))
      associate(opening_bracket => colon + index(raw_line(colon+1:), '['))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), ']'))
          associate(commas => count([(raw_line(i:i)==",", i = opening_bracket+1, closing_bracket-1)]))
            allocate(value_(commas+1))
            opening_quotes = opening_bracket + index(raw_line(opening_bracket+1:), '"')
            closing_quotes = opening_quotes + index(raw_line(opening_quotes+1:), '"')
            value_(1) = raw_line(opening_quotes+1:closing_quotes-1)
            do i = 1, commas
              comma = closing_quotes + index(raw_line(closing_quotes+1:), ',')
              opening_quotes = comma + index(raw_line(comma+1:), '"')
              closing_quotes = opening_quotes + index(raw_line(opening_quotes+1:), '"')
              value_(i+1) = raw_line(opening_quotes+1:closing_quotes-1)
            end do
          end associate
        end associate
      end associate
    end associate
  end procedure

  module procedure get_string

    character(len=:), allocatable :: raw_line

    call assert(key==self%get_json_key(), "key==self%get_string_json()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(opening_value_quotes => index(text_after_colon, '"'))
        associate(closing_value_quotes => opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"'))
          if (any([opening_value_quotes, closing_value_quotes] == 0)) then
            value_ = string_t(trim(adjustl((text_after_colon))))
          else
            value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
          end if
        end associate
      end associate
    end associate

  end procedure

  module procedure get_logical_with_character_key
    value_ = self%get_logical(string_t(key), mold)
  end procedure

  module procedure get_logical
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_logical): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        call assert(string_value=="true" .or. string_value=="false", &
          'string_s(get_logical): string_value=="true" .or. string_value="false"', string_value)
        value_ = string_value == "true"
      end associate
    end associate

  end procedure

  module procedure get_integer
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_logical): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        read(string_value, fmt=*) value_
      end associate
    end associate

  end procedure

  module procedure get_integer_with_character_key
    value_ = self%get_integer(string_t(key), mold)
  end procedure

  module procedure get_integer_array_with_character_key
    value_ = int(self%get_integer_array(string_t(key), mold))
  end procedure

  module procedure get_integer_array
    value_ = int(self%get_real_array(key,mold=[0.]))
  end procedure

  module procedure get_real_array_with_character_key
    value_ = self%get_real_array(string_t(key), mold)
  end procedure

  module procedure get_double_precision_array_with_character_key
    value_ = self%get_double_precision_array(string_t(key), mold)
  end procedure

  module procedure get_real_array
    character(len=:), allocatable :: raw_line
    real, allocatable :: real_array(:)
    integer i

    call assert(key==self%get_json_key(), "string_s(get_{real,integer}_array): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(colon => index(raw_line, ":"))
      associate(opening_bracket => colon + index(raw_line(colon+1:), "["))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), "]"))
          associate(commas => count("," == [(raw_line(i:i), i=opening_bracket+1,closing_bracket-1)]))
            associate(num_inputs => commas + 1)
              allocate(real_array(num_inputs))
              read(raw_line(opening_bracket+1:closing_bracket-1), fmt=*) real_array
              value_ = real_array
            end associate
          end associate
        end associate
      end associate
    end associate

  end procedure

  module procedure get_double_precision_array
    character(len=:), allocatable :: raw_line
    double precision, allocatable :: double_precision_array(:)
    integer i

    call assert(key==self%get_json_key(), "string_s(get_{double precision,integer}_array): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(colon => index(raw_line, ":"))
      associate(opening_bracket => colon + index(raw_line(colon+1:), "["))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), "]"))
          associate(commas => count("," == [(raw_line(i:i), i=opening_bracket+1,closing_bracket-1)]))
            associate(num_inputs => commas + 1)
              allocate(double_precision_array(num_inputs))
              read(raw_line(opening_bracket+1:closing_bracket-1), fmt=*) double_precision_array
              value_ = double_precision_array
            end associate
          end associate
        end associate
      end associate
    end associate

  end procedure

  module procedure string_t_eq_string_t
    lhs_eq_rhs = lhs%string() == rhs%string()
  end procedure
   
  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string() == rhs
  end procedure

  module procedure character_eq_string_t
    lhs_eq_rhs = lhs == rhs%string()
  end procedure
   
  module procedure string_t_ne_string_t
    lhs_ne_rhs = lhs%string() /= rhs%string()
  end procedure
   
  module procedure string_t_ne_character
    lhs_ne_rhs = lhs%string() /= rhs
  end procedure

  module procedure character_ne_string_t
    lhs_ne_rhs = lhs /= rhs%string()
  end procedure
   
  module procedure assign_string_t_to_character
    lhs = rhs%string()
  end procedure
   
  module procedure assign_character_to_string_t
    lhs%string_ = rhs
  end procedure

  module procedure string_t_cat_string_t
    lhs_cat_rhs = string_t(lhs%string_ // rhs%string_)
  end procedure
   
  module procedure string_t_cat_character
    lhs_cat_rhs = string_t(lhs%string_ // rhs)
  end procedure

  module procedure character_cat_string_t
    lhs_cat_rhs = string_t(lhs // rhs%string_)
  end procedure
   
  module procedure bracket
  
    character(len=:), allocatable :: actual_opening, actual_closing

    associate(opening_present => present(opening))

      if (opening_present) then
        actual_opening = opening
      else
        actual_opening = "["
      end if

      if (present(closing)) then
        actual_closing = closing
      else if(opening_present) then
        actual_closing = actual_opening
      else
        actual_closing = "]"
      end if

    end associate

    bracketed_self = string_t(actual_opening // self%string_ // actual_closing)

  end procedure
   
end submodule julienne_string_s
