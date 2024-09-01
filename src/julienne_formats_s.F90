! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
submodule(julienne_formats_m) julienne_formats_s
  !! Construct separated-value formats 
  implicit none

contains

#ifndef _CRAYFTN

  module procedure separated_values
    character(len=*), parameter :: prefix = "(*(G0,:,'"
    character(len=*), parameter :: suffix =           "'))"
    character(len=*), parameter :: complex_prefix = "(*('(',G0,',',G0,')',:,'" 

    select rank(mold)
      rank(1)
        select type(mold)
          type is(complex)
            format_string = complex_prefix // separator // suffix
          type is(real)
            format_string = prefix // separator // suffix
          type is(integer)
            format_string = prefix // separator // suffix
          type is(character(len=*))
            format_string = prefix // separator // suffix
          class default
             error stop "format_s separated_values: unsupported type"
        end select
      rank default
        error stop "formats_s separated_values: unsupported rank"
    end select
  end procedure

#else

  module procedure separated_values_1D
    character(len=*), parameter :: prefix = "(*(G0,:,'"
    character(len=*), parameter :: suffix =           "'))"
    character(len=*), parameter :: complex_prefix = "(*('(',G0,',',G0,')',:,'" 

    select type(mold)
      type is(complex)
        format_string = complex_prefix // separator // suffix
      type is(real)
        format_string = prefix // separator // suffix
      type is(integer)
        format_string = prefix // separator // suffix
      type is(character(len=*))
        format_string = prefix // separator // suffix
      class default
         error stop "format_s separated_values_1D: unsupported type"
    end select
  end procedure

#endif

end submodule julienne_formats_s
