! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module string_test_m
  use assert_m, only : assert
  use iso_c_binding, only : c_bool

  use julienne_m, only : &
     test_t &
    ,test_result_t &
    ,test_description_t &
    ,test_description_substring &
    ,string_t &
    ,operator(.cat.) &
    ,operator(.csv.) &
    ,operator(.sv.)

#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif

  implicit none

  private
  public :: string_test_t

  type, extends(test_t) :: string_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The string_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
      test_description_t &
        (string_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated"), check_allocation), &
      test_description_t &
        (string_t('supporting operator(==) for string_t and character operands'), supports_equivalence_operator), &
      test_description_t &
        (string_t('supporting operator(/=) for string_t and character operands'), supports_non_equivalence_operator), &
      test_description_t &
        (string_t('supporting operator(//) for string_t and character operands'), supports_concatenation_operator), &
      test_description_t &
        (string_t('assigning a string_t object to a character variable'), assigns_string_t_to_character), &
      test_description_t &
        (string_t('assigning a character variable to a string_t object'), assigns_character_to_string_t), &
      test_description_t &
        (string_t('constructing from a default integer'), constructs_from_default_integer), &
      test_description_t &
        (string_t('constructing from a default real value'), constructs_from_default_real), &
      test_description_t &
        (string_t('constructing from a double-precision value'), constructs_from_double_precision), &
      test_description_t &
        (string_t('constructing from a default-kind logical value'), constructs_from_default_logical), &
      test_description_t &
        (string_t('constructing from a logical(c_bool) value'), constructs_from_logical_c_bool), &
      test_description_t &
        (string_t('constructing from a default-precision complex value'), constructs_from_default_complex), &
      test_description_t &
        (string_t('constructing from a double-precision complex value'), constructs_from_double_precision_complex), &
      test_description_t &
        (string_t('supporting unary operator(.cat.) for array arguments'), concatenates_elements), &
      test_description_t &
        (string_t("extracting a key string from a colon-separated key/value pair"), extracts_key), &
      test_description_t &
        (string_t("extracting a real value from a colon-separated key/value pair"), extracts_real_value), &
      test_description_t &
        (string_t("extracting a double-precision value from a colon-separated key/value pair"), extracts_double_precision_value), &
      test_description_t &
        (string_t("extracting a string value from a colon-separated key/value pair"), extracts_string_value), &
      test_description_t &
        (string_t("extracting a string value from a colon-separated key/value pair"), extracts_character_value), &
      test_description_t &
        (string_t("extracting a logical value from a colon-separated key/value pair"), extracts_logical_value), &
      test_description_t &
        (string_t("extracting an integer array value from a colon-separated key/value pair"), extracts_integer_array_value), &
      test_description_t &
        (string_t("extracting a string_t array value from a colon-separated key/value pair"), extracts_string_array_value), &
      test_description_t &
        (string_t("extracting an real array value from a colon-separated key/value pair"), extracts_real_array_value), &
      test_description_t &
        (string_t("extracting a double-precision array value from a colon-separated key/value pair"), extracts_dp_array_value), &
      test_description_t &
        (string_t("extracting an integer value from a colon-separated key/value pair"), extracts_integer_value), &
      test_description_t &
        (string_t('extracting a file base name'), extracts_file_base_name), &
      test_description_t &
        (string_t('extracting a file name extension'), extracts_file_name_extension), &
      test_description_t &
        (string_t('constructing a bracketed string'), brackets_strings), &
      test_description_t &
        (string_t('constructing (comma-)separated values from character or string_t arrays'), constructs_separated_values) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: &
      check_allocation_ptr, supports_equivalence_ptr, supports_non_equivalence_ptr, supports_concatenation_ptr, &
      assigns_string_ptr, assigns_character_ptr, constructs_from_integer_ptr, constructs_from_default_real_ptr, constructs_from_double_precision_ptr, &
      constructs_from_default_logical_ptr, constructs_from_logical_c_bool_ptr, constructs_from_default_complex_ptr, &
      constructs_from_double_precision_complex_ptr, &
      concatenates_ptr, extracts_key_ptr, extracts_real_ptr, extracts_string_ptr, extracts_logical_ptr, extracts_integer_array_ptr, &
      extracts_real_array_ptr, extracts_integer_ptr, extracts_file_base_ptr, extracts_file_name_ptr, &
      extracts_string_array_ptr, &
      extracts_character_ptr, extracts_double_precision_value_ptr, extracts_dp_array_value_ptr, &
      brackets_strings_ptr, constructs_separated_values_ptr
        
    check_allocation_ptr => check_allocation
    supports_equivalence_ptr => supports_equivalence_operator
    supports_non_equivalence_ptr => supports_non_equivalence_operator
    supports_concatenation_ptr => supports_concatenation_operator
    assigns_string_ptr => assigns_string_t_to_character
    assigns_character_ptr => assigns_character_to_string_t
    constructs_from_integer_ptr => constructs_from_default_integer
    constructs_from_double_precision_ptr => constructs_from_double_precision
    constructs_from_default_real_ptr => constructs_from_default_real
    constructs_from_default_logical_ptr => constructs_from_default_logical
    constructs_from_logical_c_bool_ptr => constructs_from_logical_c_bool
    constructs_from_default_complex_ptr => constructs_from_default_complex
    constructs_from_double_precision_complex_ptr => constructs_from_double_precision_complex
    concatenates_ptr => concatenates_elements
    extracts_key_ptr => extracts_key
    extracts_real_ptr => extracts_real_value
    extracts_double_precision_value_ptr => extracts_double_precision_value
    extracts_string_ptr => extracts_string_value
    extracts_character_ptr => extracts_character_value
    extracts_logical_ptr => extracts_logical_value
    extracts_integer_array_ptr  => extracts_integer_array_value
    extracts_string_array_ptr  => extracts_string_array_value
    extracts_real_array_ptr => extracts_real_array_value
    extracts_dp_array_value_ptr => extracts_dp_array_value
    extracts_integer_ptr => extracts_integer_value
    extracts_file_base_ptr => extracts_file_base_name
    extracts_file_name_ptr => extracts_file_name_extension
    brackets_strings_ptr => brackets_strings
    constructs_separated_values_ptr => constructs_separated_values

    test_descriptions = [ & 
      test_description_t( &
        string_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated"), check_allocation_ptr), &
      test_description_t(string_t('supporting operator(==) for string_t and character operands'), supports_equivalence_ptr), &
      test_description_t( &
        string_t('supporting operator(/=) for string_t and character operands'), supports_non_equivalence_ptr), &
      test_description_t( &
        string_t('supporting operator(//) for string_t and character operands'), supports_concatenation_ptr), &
      test_description_t(string_t('assigning a string_t object to a character variable'), assigns_string_ptr), &
      test_description_t(string_t('assigning a character variable to a string_t object'), assigns_character_ptr), &
      test_description_t(string_t('constructing from a default integer'), constructs_from_integer_ptr), &
      test_description_t(string_t('constructing from a default real value'), constructs_from_default_real_ptr), &
      test_description_t(string_t('constructing from a double-precision value'), constructs_from_double_precision_ptr), &
      test_description_t(string_t('constructing from a default-kind logical value'), constructs_from_default_logical_ptr), &
      test_description_t(string_t('constructing from a logical(c_bool) value'), constructs_from_logical_c_bool_ptr), &
      test_description_t(string_t('constructing from a default-precision complex value'), constructs_from_default_complex_ptr), &
      test_description_t(string_t('constructing from a double-precision complex value'), constructs_from_double_precision_complex_ptr), &
      test_description_t(string_t('supporting unary operator(.cat.) for array arguments'), concatenates_ptr), &
      test_description_t(string_t("extracting a key string from a colon-separated key/value pair"), extracts_key_ptr), &
      test_description_t(string_t("extracting a real value from a colon-separated key/value pair"), extracts_real_ptr), &
      test_description_t( &
        string_t("extracting a double-precision value from a colon-separated key/value pair"), extracts_double_precision_value_ptr),&
      test_description_t(string_t("extracting a string value from a colon-separated key/value pair"), extracts_string_ptr), &
      test_description_t(string_t("extracting a character value from a colon-separated key/value pair"), extracts_character_ptr), &
      test_description_t(string_t("extracting a logical value from a colon-separated key/value pair"), extracts_logical_ptr), &
      test_description_t( &
        string_t("extracting an integer array value from a colon-separated key/value pair"), extracts_integer_array_ptr), &
      test_description_t( &
        string_t("extracting an string array value from a colon-separated key/value pair"), extracts_string_array_ptr), &
      test_description_t( &
        string_t("extracting an real array value from a colon-separated key/value pair"), extracts_real_array_ptr), &
      test_description_t( &
        string_t("extracting an double-precision array value from a colon-separated key/value pair"), extracts_dp_array_value_ptr), &
      test_description_t(string_t("extracting an integer value from a colon-separated key/value pair"), extracts_integer_ptr), &
      test_description_t(string_t('extracting a file base name'), extracts_file_base_ptr), &
      test_description_t(string_t('extracting a file name extension'), extracts_file_name_ptr), &
      test_description_t(string_t('constructing a bracketed string'), brackets_strings_ptr),&
      test_description_t(string_t('constructing (comma-)separated values from string_t arrays'), constructs_separated_values_ptr) &
    ]   
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  pure function check_allocation() result(passed)
    type(string_t) :: scalar_not_allocated, scalar_allocated, array_allocated(2), array_not_allocated(2)
    logical passed

    scalar_allocated = string_t("")
    array_allocated = [string_t("yada yada"), string_t("blah blah blah")]
    passed = (.not. any([scalar_not_allocated%is_allocated(), array_not_allocated%is_allocated()])) .and. &
             (all([scalar_allocated%is_allocated(), array_allocated%is_allocated()]))
  end function

  function extracts_key() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"foo" : "bar"'))
      passed = line%get_json_key() == string_t("foo")
    end associate
#else
    block
      type(string_t) line
      line = string_t('"foo" : "bar"')
      passed = line%get_json_key() == string_t("foo")
    end block
#endif
  end function

  function extracts_double_precision_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"pi" : 3.141592653589793D0'))
      passed = line%get_json_value(key="pi", mold=0.D0) == 3.141592653589793D0
    end associate
#else
    block
      type(string_t) line
      line = string_t('"pi" : 3.141592653589793D0')
      passed = line%get_json_value(key="pi", mold=0.D0) == 3.141592653589793D0
    end block
#endif
  end function

  function extracts_real_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"pi" : 3.14159'))
      passed = line%get_json_value(key=string_t("pi"), mold=1.) == 3.14159
    end associate
#else
    block
      type(string_t) line
      line = string_t('"pi" : 3.14159')
      passed = line%get_json_value(key=string_t("pi"), mold=1.) == 3.14159
    end block
#endif
  end function

  function extracts_character_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"foo" : "bar"'), line_with_comma => string_t('"foo" : "bar",'))
      passed = line%get_json_value(key=string_t("foo"), mold="") == "bar" .and. &
               line%get_json_value(key="foo" , mold="") == "bar" .and. &
               line_with_comma%get_json_value(key=string_t("foo"), mold="") == "bar" .and. &
               line_with_comma%get_json_value(key="foo" , mold="") == "bar"
    end associate
#else
    block
      type(string_t) line, line_with_comma
      line = string_t('"foo" : "bar"')
      line_with_comma = string_t('"foo" : "bar",')
      passed = line%get_json_value(key=string_t("foo"), mold="") == "bar" .and. &
               line%get_json_value(key="foo" , mold="") == "bar" .and. &
               line_with_comma%get_json_value(key=string_t("foo"), mold="") == "bar" .and. &
               line_with_comma%get_json_value(key="foo" , mold="") == "bar"
    end block
#endif
  end function

  function extracts_string_value() result(passed)
    logical passed
    
#ifndef _CRAYFTN
    associate(line => string_t('"foo" : "bar"'))
      passed = line%get_json_value(key=string_t("foo"), mold=string_t("")) == string_t("bar")
    end associate
#else
    block
      type(string_t) line
      line = string_t('"foo" : "bar"')
      passed = line%get_json_value(key=string_t("foo"), mold=string_t("")) == string_t("bar")
    end block
#endif
  end function

  function extracts_integer_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"an integer" : 99'))
      passed = line%get_json_value(key=string_t("an integer"), mold=0) == 99
    end associate
#else
    block
      type(string_t) line
      line = string_t('"an integer" : 99')
      passed = line%get_json_value(key=string_t("an integer"), mold=0) == 99
    end block
#endif
  end function

  function extracts_logical_value() result(passed)
    logical passed
    
#ifndef _CRAYFTN
    associate( &
      key_true_pair => string_t('"yada yada" : true'), &
      key_false_pair => string_t('"blah blah" : false'), &
      trailing_comma => string_t('"trailing comma" : true,') &
    )
      associate( &
         true => key_true_pair%get_json_value(key=string_t("yada yada"), mold=.true.), &
         false => key_false_pair%get_json_value(key=string_t("blah blah"), mold=.true.), &
         true_too => trailing_comma%get_json_value(key=string_t("trailing comma"), mold=.true.) &
      )
        passed = true .and. true_too .and. .not. false
      end associate
    end associate
#else
    block
      type(string_t) key_true_pair, key_false_pair, trailing_comma
      logical  true, false, true_too

      key_true_pair = string_t('"yada yada" : true')
      key_false_pair = string_t('"blah blah" : false')
      trailing_comma = string_t('"trailing comma" : true,')

      true = key_true_pair%get_json_value(key=string_t("yada yada"), mold=.true.)
      false = key_false_pair%get_json_value(key=string_t("blah blah"), mold=.true.)
      true_too = trailing_comma%get_json_value(key=string_t("trailing comma"), mold=.true.)

      passed = true .and. true_too .and. .not. false
    end block
#endif
  end function

  function extracts_string_array_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(key_string_array_pair => string_t('"lead singer" : ["stevie", "ray", "vaughn"],'))
      associate(string_array => key_string_array_pair%get_json_value(key="lead singer", mold=[string_t::]))
#ifndef __GFORTRAN__
        associate(string_array_ => key_string_array_pair%get_json_value(key=string_t("lead singer"), mold=[string_t::]))
#endif
          passed = all(string_array == [string_t("stevie"), string_t("ray"), string_t("vaughn")])
#ifndef __GFORTRAN__
          passed = passed .and. all(string_array_ == [string_t("stevie"), string_t("ray"), string_t("vaughn")])
        end associate
#endif
      end associate
    end associate
#else
    block
      type(string_t) key_string_array_pair
      type(string_t), allocatable :: string_array(:)
      key_string_array_pair = string_t('"lead singer" : ["ella", "fitzgerald"],')
      string_array = key_string_array_pair%get_json_value(key=string_t("lead singer"), mold=[string_t::])
      passed = all(string_array == [string_t("ella"), string_t("fitzgerald")])
    end block
#endif
  end function

  function extracts_integer_array_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(key_integer_array_pair => string_t('"some key" : [1, 2, 3],'))
      associate(integer_array => key_integer_array_pair%get_json_value(key=string_t("some key"), mold=[integer::]))
        passed = all(integer_array == [1, 2, 3])
      end associate
    end associate
#else
    block
      type(string_t) key_integer_array_pair
      integer, allocatable :: integer_array(:)
      key_integer_array_pair = string_t('"some key" : [1, 2, 3],')
      integer_array = key_integer_array_pair%get_json_value(key=string_t("some key"), mold=[integer::])
      passed = all(integer_array == [1, 2, 3])
    end block
#endif
  end function

  function extracts_real_array_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(key_real_array_pair => string_t('"a key" : [1., 2., 4.],'))
      associate(real_array => key_real_array_pair%get_json_value(key=string_t("a key"), mold=[real::]))
        passed = all(real_array == [1., 2., 4.])
      end associate
    end associate
#else
    block
      type(string_t) key_real_array_pair
      real, allocatable :: real_array(:)
      key_real_array_pair = string_t('"a key" : [1., 2., 4.],')
      real_array = key_real_array_pair%get_json_value(key=string_t("a key"), mold=[real::])
      passed = all(real_array == [1., 2., 4.])
    end block
#endif
  end function

  function extracts_dp_array_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(key_dp_array_pair => string_t('"a key" : [1.D0, 2.D0, 4.D0],'))
      associate(dp_array => key_dp_array_pair%get_json_value(key=string_t("a key"), mold=[double precision::]))
        passed = all(dp_array == [1.D0, 2.D0, 4.D0])
      end associate
    end associate
#else
    block
      type(string_t) key_dp_array_pair
      double  precision, allocatable :: dp_array(:)
      key_dp_array_pair = string_t('"a key" : [1., 2., 4.],')
      dp_array = key_dp_array_pair%get_json_value(key=string_t("a key"), mold=[double precision::])
      passed = all(dp_array == [1D0, 2D0, 4D0])
    end block
#endif
  end function

  function supports_equivalence_operator() result(passed)
    logical passed
    passed = &
      string_t("abcdefg") == string_t("abcdefg") .and. &
      string_t("xyz pdq") ==          "xyz pdq"  .and. &
               "123.456"  == string_t("123.456")
  end function

  function supports_non_equivalence_operator() result(passed)
    logical passed
    passed = &
      string_t("abcdefg") /= string_t("xyz pdq") .and. &
      string_t("xyz pdq") /=          "abcdefg"  .and. &
               "123.456"  /= string_t("456.123")
  end function

  function assigns_string_t_to_character() result(passed)
    logical passed
    character(len=:), allocatable :: lhs

    associate(rhs => string_t("ya don't say"))
      lhs = rhs
      passed = lhs == rhs
    end associate
  end function

  function assigns_character_to_string_t() result(passed)
    logical passed
    character(len=*), parameter :: rhs = "well, alrighty then"
    type(string_t) lhs

    lhs = rhs
    passed = lhs == rhs
  end function

  function supports_concatenation_operator() result(passed)
    logical passed
    character(len=*), parameter :: prefix = "foo", postfix="bar"

#ifndef _CRAYFTN
    associate(infix => string_t(" yada yada "))
      passed = prefix // infix // postfix == prefix // infix%string() // postfix 
    end associate
#else
    block
      type(string_t) infix
      infix = string_t(" yada yada ")
      passed = prefix // infix // postfix == prefix // infix%string() // postfix 
    end block
#endif
  end function

  function constructs_from_default_integer() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(string => string_t(1234567890))
      passed = adjustl(trim(string%string())) == "1234567890"
    end associate
#else
    block 
      type(string_t) string
      string = string_t(1234567890)
      passed = adjustl(trim(string%string())) == "1234567890"
    end block
#endif
  end function

  function constructs_from_default_real() result(passed)
    logical passed
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 for an exactly representable rational number
    real read_value
    character(len=:), allocatable :: character_representation

#ifndef _CRAYFTN
    associate(string => string_t(real_value))
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == real_value
    end associate
#else
    block
      type(string_t) string
      string = string_t(real_value)
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == real_value
    end block
#endif
  end function

  function constructs_from_double_precision() result(passed)
    logical passed
    double precision, parameter :: double_precision_value = -1D0/1024D0 ! use a negative power of 2 for an exactly representable rational number
    real read_value
    character(len=:), allocatable :: character_representation

#ifndef _CRAYFTN
    associate(string => string_t(double_precision_value))
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == double_precision_value
    end associate
#else
    block
      type(string_t) string
      string = string_t(real_value)
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == real_value
    end block
#endif
  end function

  function constructs_from_default_complex() result(passed)
    logical passed
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 for an exactly representable rational number
    complex, parameter :: z = (real_value, real_value)
    complex read_value
    character(len=:), allocatable :: character_representation

#ifndef _CRAYFTN
    associate(string => string_t(z))
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == z
    end associate
#else
    block
      type(string_t) string
      string = string_t(z)
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == z
    end block
#endif
  end function

  function constructs_from_double_precision_complex() result(passed)
    logical passed
    double precision, parameter :: double_precision_value = -1D0/1024D0 ! use a negative power of 2 for an exactly representable rational number
    complex(kind(1D0)), parameter :: z = (double_precision_value, double_precision_value)
    complex(kind(1D0)) read_value
    character(len=:), allocatable :: character_representation

#ifndef _CRAYFTN
    associate(string => string_t(z))
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == z
    end associate
#else
    block
      type(string_t) string
      string = string_t(z)
      character_representation = string%string()
      read(character_representation, *) read_value
      passed = read_value == z
    end block
#endif
  end function

  function constructs_from_default_logical() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(true => string_t(.true.), false => string_t(.false.))
      passed = true%string() == "T" .and. false%string() == "F"
    end associate
#else
    block
      type(string_t) true, false
      true = string_t(.true.)
      false = string_t(.false.)
      passed = string%string() == "T" .and. false%string() == "F"
    end block
#endif
  end function

  function constructs_from_logical_c_bool() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(true => string_t(.true._c_bool), false => string_t(.false._c_bool))
      passed = true%string() == "T" .and. false%string() == "F"
    end associate
#else
    block
      type(string_t) true, false
      true = string_t(.true._c_bool)
      false = string_t(.false._c_bool)
      passed = string%string() == "T" .and. false%string() == "F"
    end block
#endif
  end function

  function extracts_file_base_name() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(string => string_t(" foo .bar.too "))
      passed = string%base_name() == "foo .bar"
    end associate
#else
    block
      type(string_t) string
      string = string_t(" foo .bar.too ")
      passed = string%base_name() == "foo .bar"
    end block
#endif
  end function

  function extracts_file_name_extension() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(string => string_t(" foo .bar.too "))
      passed = string%file_extension() == "too"
    end associate
#else
    block
      type(string_t) string
      string = string_t(" foo .bar.too ")
      passed = string%file_extension() == "too"
    end block
#endif
  end function

  function concatenates_elements() result(passed)
    logical passed
    passed = (.cat. [string_t("foo"), string_t("bar")]) == "foobar"
  end function

  function brackets_strings() result(passed)
    logical passed

#ifdef __GFORTRAN__
    type(string_t), allocatable :: array(:)
    array  = string_t(["do", "re", "mi"])
#endif

    associate( &
       scalar => string_t("do re mi") &
#ifndef __GFORTRAN__
      ,array  => string_t(["do", "re", "mi"]) &
#endif
    )
      passed = &
                 scalar%bracket()        == string_t("[do re mi]")                                  &
        .and. all(array%bracket()        == [string_t("[do]"), string_t("[re]"), string_t("[mi]")]) &
        .and. all(array%bracket('"')     == [string_t('"do"'), string_t('"re"'), string_t('"mi"')]) &
        .and. all(array%bracket("{","}") == [string_t('{do}'), string_t('{re}'), string_t('{mi}')])
    end associate
  end function

  function constructs_separated_values() result(passed)
    logical passed
    passed = &
            "a,bc,def" == .csv. [string_t("a"), string_t("bc"), string_t("def")]    &
      .and. "abc,def"  == .csv. ["abc", "def"]                                      &
      .and. "do|re|mi" == (string_t(["do", "re", "mi"])         .sv.          "|" ) &
      .and. "dore|mi"  == (([string_t("dore"), string_t("mi")]) .sv. string_t("|")) &
      .and. "do|re|mi" == (         ["do", "re", "mi"]          .sv.          "|" ) &
      .and. "do|re|mi" == (         ["do", "re", "mi"]          .sv. string_t("|"))
  end function

end module string_test_m
