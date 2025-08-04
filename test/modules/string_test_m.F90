! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
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
    ,test_diagnosis_t &
    ,string_t &
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    ,diagnosis_function_i &
#endif
    ,operator(.cat.) &
    ,operator(.csv.) &
    ,operator(.sv.)

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
       test_description_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated", check_allocation)&
      ,test_description_t("extracting a key string from a colon-separated key/value pair",                            extracts_key)&
      ,test_description_t("extracting double-precision value from colon-separated key/value pair", extracts_double_precision_value)&
      ,test_description_t("extracting a real value from a colon-separated key/value pair",                     extracts_real_value)&
      ,test_description_t("extracting a character value from a colon-separated key/value pair",           extracts_character_value)&
      ,test_description_t("extracting a string value from a colon-separated key/value pair",                 extracts_string_value)&
      ,test_description_t("extracting an integer value from a colon-separated key/value pair",              extracts_integer_value)&
      ,test_description_t("extracting a logical value from a colon-separated key/value pair",               extracts_logical_value)&
      ,test_description_t("extracting an integer array value from a colon-separated key/value pair",  extracts_integer_array_value)&
      ,test_description_t("extracting an real array value from a colon-separated key/value pair",        extracts_real_array_value)&
      ,test_description_t("extracting a double-precision array from a colon-separated key/value pair",     extracts_dp_array_value)&
      ,test_description_t('supporting operator(==) for string_t and character operands',             supports_equivalence_operator)&
      ,test_description_t('supporting operator(/=) for string_t and character operands',         supports_non_equivalence_operator)&
      ,test_description_t('assigning a string_t object to a character variable',                     assigns_string_t_to_character)&
      ,test_description_t('assigning a character variable to a string_t object',                     assigns_character_to_string_t)&
      ,test_description_t('supporting operator(//) for string_t and character operands',           supports_concatenation_operator)&
      ,test_description_t('constructing from a default integer',                                   constructs_from_default_integer)&
      ,test_description_t('constructing from a default real value',                                   constructs_from_default_real)&
      ,test_description_t('constructing from a double-precision value',                           constructs_from_double_precision)&
      ,test_description_t('constructing from a default-precision complex value',                   constructs_from_default_complex)&
      ,test_description_t('constructing from a default-kind logical value',                        constructs_from_default_logical)&
      ,test_description_t('constructing from a logical(c_bool) value',                              constructs_from_logical_c_bool)&
      ,test_description_t('extracting a file base name',                                                   extracts_file_base_name)&
      ,test_description_t('extracting a file name extension',                                         extracts_file_name_extension)&
      ,test_description_t('supporting unary operator(.cat.) for array arguments',                            concatenates_elements)&
      ,test_description_t('constructing bracketed strings',                                                       brackets_strings)&
      ,test_description_t("extracting a string_t array value from a colon-separated key/value pair",   extracts_string_array_value)&
      ,test_description_t('constructing (comma-)separated values from character or string_t arrays',   constructs_separated_values)&
      ,test_description_t('constructing from a double-precision complex value',           constructs_from_double_precision_complex)&
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(diagnosis_function_i), pointer :: &
      ,check_allocation_ptr                         => check_allocation &
      ,extracts_key_ptr                             => extracts_key &
      ,extracts_double_precision_value_ptr          => extracts_double_precision_value &
      ,extracts_real_value_ptr                      => extracts_real_value &
      ,extracts_character_value_ptr                 => extracts_character_value &
      ,extracts_string_value_ptr                    => extracts_string_value &
      ,extracts_integer_value_ptr                   => extracts_integer_value &
      ,extracts_logical_value_ptr                   => extracts_logical_value &
      ,extracts_integer_array_value_ptr             => extracts_integer_array_value &
      ,extracts_real_array_value_ptr                => extracts_real_array_value &
      ,extracts_dp_array_value_ptr                  => extracts_dp_array_value &
      ,supports_equivalence_operator_ptr            => supports_equivalence_operator &
      ,supports_non_equivalence_operator_ptr        => supports_non_equivalence_operator &
      ,assigns_string_t_to_character_ptr            => assigns_string_t_to_character &
      ,assigns_character_to_string_t_ptr            => assigns_character_to_string_t &
      ,supports_concatenation_operator_ptr          => supports_concatenation_operator &
      ,constructs_from_default_integer_ptr          => constructs_from_default_integer &
      ,constructs_from_default_real_ptr             => constructs_from_default_real &
      ,constructs_from_double_precision_ptr         => constructs_from_double_precision &
      ,constructs_from_default_complex_ptr          => constructs_from_default_complex &
      ,constructs_from_default_logical_ptr          => constructs_from_default_logical &
      ,constructs_from_logical_c_bool_ptr           => constructs_from_logical_c_bool &
      ,extracts_file_base_name_ptr                  => extracts_file_base_name &
      ,extracts_file_name_extension_ptr             => extracts_file_name_extension &
      ,concatenates_elements_ptr                    => concatenates_elements &
      ,brackets_strings_ptr                         => brackets_strings &
      ,extracts_string_array_value_ptr              => extracts_string_array_value &
      ,constructs_separated_values_ptr              => constructs_separated_values &
      ,constructs_from_double_precision_complex_ptr => constructs_from_double_precision_complex

    test_descriptions = [ &
       test_description_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated", check_allocation_ptr)&
      ,test_description_t("extracting a key string from a colon-separated key/value pair",                            extracts_key_ptr)&
      ,test_description_t("extracting double-precision value from colon-separated key/value pair", extracts_double_precision_value_ptr)&
      ,test_description_t("extracting a real value from a colon-separated key/value pair",                     extracts_real_value_ptr)&
      ,test_description_t("extracting a character value from a colon-separated key/value pair",           extracts_character_value_ptr)&
      ,test_description_t("extracting a string value from a colon-separated key/value pair",                 extracts_string_value_ptr)&
      ,test_description_t("extracting an integer value from a colon-separated key/value pair",              extracts_integer_value_ptr)&
      ,test_description_t("extracting a logical value from a colon-separated key/value pair",               extracts_logical_value_ptr)&
      ,test_description_t("extracting an integer array value from a colon-separated key/value pair",  extracts_integer_array_value_ptr)&
      ,test_description_t("extracting an real array value from a colon-separated key/value pair",        extracts_real_array_value_ptr)&
      ,test_description_t("extracting a double-precision array from a colon-separated key/value pair",     extracts_dp_array_value_ptr)&
      ,test_description_t('supporting operator(==) for string_t and character operands',             supports_equivalence_operator_ptr)&
      ,test_description_t('supporting operator(/=) for string_t and character operands',         supports_non_equivalence_operator_ptr)&
      ,test_description_t('assigning a string_t object to a character variable',                     assigns_string_t_to_character_ptr)&
      ,test_description_t('assigning a character variable to a string_t object',                     assigns_character_to_string_t_ptr)&
      ,test_description_t('supporting operator(//) for string_t and character operands',           supports_concatenation_operator_ptr)&
      ,test_description_t('constructing from a default integer',                                   constructs_from_default_integer_ptr)&
      ,test_description_t('constructing from a default real value',                                   constructs_from_default_real_ptr)&
      ,test_description_t('constructing from a double-precision value',                           constructs_from_double_precision_ptr)&
      ,test_description_t('constructing from a default-precision complex value',                   constructs_from_default_complex_ptr)&
      ,test_description_t('constructing from a default-kind logical value',                        constructs_from_default_logical_ptr)&
      ,test_description_t('constructing from a logical(c_bool) value',                              constructs_from_logical_c_bool_ptr)&
      ,test_description_t('extracting a file base name',                                                   extracts_file_base_name_ptr)&
      ,test_description_t('extracting a file name extension',                                         extracts_file_name_extension_ptr)&
      ,test_description_t('supporting unary operator(.cat.) for array arguments',                            concatenates_elements_ptr)&
      ,test_description_t("extracting a string_t array value from a colon-separated key/value pair",   extracts_string_array_value_ptr)&
      ,test_description_t('constructing (comma-)separated values from character or string_t arrays',   constructs_separated_values_ptr)&
      ,test_description_t('constructing from a double-precision complex value',           constructs_from_double_precision_complex_ptr)&
      ,test_description_t('constructing bracketed strings',                                                       brackets_strings_ptr) &
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  pure function check_allocation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(string_t) :: scalar_not_allocated, scalar_allocated, array_allocated(2), array_not_allocated(2)

    scalar_allocated = string_t("")
    array_allocated = [string_t("yada yada"), string_t("blah blah blah")]

    associate(not_any_allocated => .not. any([scalar_not_allocated%is_allocated(), array_not_allocated%is_allocated()]))
      associate(all_allocated => all([scalar_allocated%is_allocated(), array_allocated%is_allocated()]))
        test_diagnosis = test_diagnosis_t( &
           test_passed = not_any_allocated .and. all_allocated &
          ,diagnostics_string = "expected .true., true.; actual " // string_t(not_any_allocated) // string_t(all_allocated) &
        )
      end associate
    end associate
  end function

  function extracts_key() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(line => string_t('"foo" : "bar"'))
      associate(key => line%get_json_key())
        test_diagnosis = test_diagnosis_t( &
           test_passed = key == string_t("foo") &
          ,diagnostics_string = "expected 'foo'; actual " // key%string() &
        )
      end associate
    end associate
  end function

  function extracts_double_precision_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: tolerance = 1D-16

    associate(line => string_t('"pi" : 3.141592653589793D0'))
      associate(json_value => line%get_json_value(key="pi", mold=0.D0))
        test_diagnosis = test_diagnosis_t( &
           test_passed = abs(json_value - 3.141592653589793D0) < tolerance &
          ,diagnostics_string = "expected 3.141592653589793D0, actual " // string_t(json_value) &
        )
      end associate
    end associate
  end function

  function extracts_real_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    real, parameter :: tolerance = 1E-08

    associate(line => string_t('"pi" : 3.14159'))
      associate(json_value => line%get_json_value(key=string_t("pi"), mold=1.))
        test_diagnosis = test_diagnosis_t( &
           test_passed = json_value == 3.14159 &
          ,diagnostics_string = "expected 3.14159, actual " // string_t(json_value) &
        )
      end associate
    end associate
  end function

  function extracts_character_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(line => string_t('"foo" : "bar"'), line_with_comma => string_t('"foo" : "bar",'))
      associate(json_values => [ &
           line%get_json_value(key="foo" , mold="") &
          ,line%get_json_value(key=string_t("foo"), mold="") &
          ,line_with_comma%get_json_value(key="foo" , mold="") &
          ,line_with_comma%get_json_value(key=string_t("foo"), mold="") &
      ])
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(json_values == "bar") &
          ,diagnostics_string = "expected bar; actual " // .csv. json_values &
        )
      end associate
    end associate
  end function

  function extracts_string_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(line => string_t('"foo" : "bar"'))
      associate(json_value => line%get_json_value(key=string_t("foo"), mold=string_t("")))
        test_diagnosis = test_diagnosis_t( &
          test_passed =  json_value == "bar", &
          diagnostics_string = "expected 'bar', actual " // json_value &
        )
      end associate
    end associate

  end function

  function extracts_integer_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(line => string_t('"an integer" : 99'))
      associate(json_value => line%get_json_value(key=string_t("an integer"), mold=0))
        test_diagnosis = test_diagnosis_t( &
           test_passed = json_value == 99 &
          ,diagnostics_string = "expected 99, actual " // string_t(json_value) &
        )
      end associate
    end associate
  end function

  function extracts_logical_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate( &
      key_true_pair => string_t('"yada yada" : true'), &
      key_false_pair => string_t('"blah blah" : false'), &
      trailing_comma => string_t('"trailing comma" : true,') &
    )
      associate( &
         true => key_true_pair%get_json_value(key=string_t("yada yada"), mold=.true.) &
        ,true_too => trailing_comma%get_json_value(key=string_t("trailing comma"), mold=.true.) &
        ,false => key_false_pair%get_json_value(key=string_t("blah blah"), mold=.true.) &
      )
        test_diagnosis = test_diagnosis_t( &
           test_passed = all([true, true_too, .not. false]) &
          ,diagnostics_string = "expected T,T,T; actual  " // .csv. string_t([true, true_too, .not. false]) &
        )
      end associate
    end associate
  end function

  function extracts_string_array_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(key_string_array_pair => string_t('"lead singer" : ["stevie", "ray", "vaughn"],'))
      associate(string_array => key_string_array_pair%get_json_value(key="lead singer", mold=[string_t::]))
        associate(expected_value => [string_t("stevie"), string_t("ray"), string_t("vaughn")])
          test_diagnosis = test_diagnosis_t( &
             test_passed = all(string_array == expected_value) &
            ,diagnostics_string = "expected " // .csv. expected_value //"; actual " //.csv. string_array &
          )
        end associate
      end associate
    end associate
  end function

  function extracts_integer_array_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(key_integer_array_pair => string_t('"some key" : [1, 2, 3],'))
      associate(integer_array => key_integer_array_pair%get_json_value(key=string_t("some key"), mold=[integer::]))
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(integer_array == [1, 2, 3]) &
          ,diagnostics_string = "expected 1,2,3; actual " // .csv. string_t(integer_array) &
        )
      end associate
    end associate
  end function

  function extracts_real_array_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: tolerance = 1E-08

    associate(key_real_array_pair => string_t('"a key" : [1., 2., 4.],'))
      associate(real_array => key_real_array_pair%get_json_value(key=string_t("a key"), mold=[real::]))
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(abs(real_array - [1., 2., 4.]) < tolerance) &
          ,diagnostics_string = "expected 1,2,3; actual " // .csv. string_t(real_array) &
        )
      end associate
    end associate
  end function

  function extracts_dp_array_value() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: tolerance = 1E-16

    associate(key_dp_array_pair => string_t('"a key" : [1.D0, 2.D0, 4.D0],'))
      associate(dp_array => key_dp_array_pair%get_json_value(key=string_t("a key"), mold=[double precision::]))
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(abs(dp_array - [1D0, 2D0, 4D0]) < tolerance) &
          ,diagnostics_string = "expected 1.,2.,3.; actual " // .csv. string_t(dp_array) &
        )
      end associate
    end associate
  end function

  function supports_equivalence_operator() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(comparisons => [ string_t("abcdefg") == string_t("abcdefg") &
                              ,string_t("xyz pdq") ==          "xyz pdq"  &
                              ,         "123.456"  == string_t("123.456") &
                              ,         "123.456"  == string_t("123"    )])
      test_diagnosis = test_diagnosis_t( &
         test_passed = all(comparisons .eqv. [.true.,.true.,.true.,.false.]) &
        ,diagnostics_string = "expected T,T,T,F; actual " // .csv. string_t([comparisons(1:3), .not. comparisons(4)]) &
      )
    end associate
  end function

  function supports_non_equivalence_operator() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(non_equivalent_strings => [string_t("abcdefg") /= string_t("xyz pdq") &
                                        ,string_t("xyz pdq") /=          "abcdefg"  &
                                        ,         "123.456"  /= string_t("456.123") &
                                        ,         "123.456"  /= string_t("123.456")])
      test_diagnosis = test_diagnosis_t( &
         test_passed = all(non_equivalent_strings .eqv. [.true.,.true.,.true.,.false.]) &
        ,diagnostics_string = "expected T,T,T,F; actual " // .csv. string_t(non_equivalent_strings) &
      )
    end associate
  end function

  function assigns_string_t_to_character() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=:), allocatable :: lhs

    associate(rhs => string_t("ya don't say"))
      lhs = rhs
      test_diagnosis = test_diagnosis_t( &
         test_passed = lhs == rhs &
        ,diagnostics_string = "expected lhs == rhs; actual lhs = " // lhs // ", rhs = " // rhs &
      )
    end associate
  end function

  function assigns_character_to_string_t() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: rhs = "well, alrighty then"
    type(string_t) lhs

    lhs = rhs
    test_diagnosis = test_diagnosis_t( &
       test_passed = lhs == rhs &
      ,diagnostics_string = "expected lhs == rhs; actual lhs = " // lhs // ", rhs = " // rhs &
    )
  end function

  function supports_concatenation_operator() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: prefix = "foo", postfix="bar", expected = "foo yada yada bar"

    associate(infix => string_t(" yada yada "))
      associate(string_string_string => prefix // infix // postfix, string_character_string => prefix // infix%string() // postfix)
        test_diagnosis = test_diagnosis_t( &
           test_passed = all([string_string_string == expected, string_character_string == expected]) &
          ,diagnostics_string = "expected '"// expected // "', actual " // string_string_string // "," // string_character_string &
        )
      end associate
    end associate
  end function

  function constructs_from_default_integer() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: expected_value = 1234567890

    associate(string => string_t(expected_value))
      test_diagnosis = test_diagnosis_t( &
         test_passed = adjustl(trim(string%string())) == "1234567890" &
        ,diagnostics_string = "expected '"// string_t(expected_value) // "', actual " // string%string() &
      )
    end associate
  end function

  function constructs_from_default_real() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 for an exactly representable rational number
    real read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(real_value))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = read_value == real_value &
        ,diagnostics_string = "expected '"// string_t(real_value) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_double_precision() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: double_precision_value = -1D0/1024D0 ! use a negative power of 2 for an exactly representable rational number
    real read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(double_precision_value))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = read_value == double_precision_value &
        ,diagnostics_string = "expected '"// string_t(double_precision_value) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_default_complex() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 for an exactly representable rational number
    real, parameter :: tolerance = 1E-08
    complex, parameter :: z = (real_value, real_value)
    complex read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(z))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = abs(read_value - z) < tolerance &
        ,diagnostics_string = "expected '"// string_t(z) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_double_precision_complex() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: double_precision_value = -1D0/1024D0 ! use a negative power of 2 for an exactly representable rational number
    double precision, parameter :: tolerance = 1E-16
    complex(kind(1D0)), parameter :: z = (double_precision_value, double_precision_value)
    complex(kind(1D0)) read_value
    character(len=:), allocatable :: character_representation

    associate(string => string_t(z))
      character_representation = string%string()
      read(character_representation, *) read_value
      test_diagnosis = test_diagnosis_t( &
         test_passed = abs(read_value - z) < tolerance &
        ,diagnostics_string = "expected '"// string_t(z) // "', actual " // string_t(read_value) &
      )
    end associate
  end function

  function constructs_from_default_logical() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(true => string_t(.true.), false => string_t(.false.))
      test_diagnosis = test_diagnosis_t( &
         test_passed = all([true%string() == "T", false%string() == "F"]) &
        ,diagnostics_string = "expected T, F; actual '"// true%string() // ", " // false%string() &
      )
    end associate
  end function

  function constructs_from_logical_c_bool() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(true => string_t(.true._c_bool), false => string_t(.false._c_bool))
      test_diagnosis = test_diagnosis_t( &
         test_passed = true%string() == "T" .and. false%string() == "F" &
        ,diagnostics_string = "expected T, F; actual '"// true%string() // ", " // false%string() &
      )
    end associate
  end function

  function extracts_file_base_name() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: expected = "foo .bar"

    associate(string => string_t(" foo .bar.too "))
      associate(base_name => string%base_name())
        test_diagnosis = test_diagnosis_t( &
           test_passed = base_name == expected &
          ,diagnostics_string = "expected "// expected // ", actual " // base_name &
        )
      end associate
    end associate
  end function

  function extracts_file_name_extension() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: expected = "too"

    associate(string => string_t(" foo .bar.too "))
      associate(file_extension => string%file_extension())
        test_diagnosis = test_diagnosis_t( &
           test_passed = file_extension == expected &
          ,diagnostics_string = "expected "// expected // ", actual " // file_extension&
        )
      end associate
    end associate
  end function

  function concatenates_elements() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*), parameter :: expected = "foobar"

    associate(cat_foo_bar => .cat. [string_t("foo"), string_t("bar")])
      test_diagnosis = test_diagnosis_t( &
         test_passed = cat_foo_bar == expected &
        ,diagnostics_string = "expected "// expected // ", actual " // cat_foo_bar &
      )
    end associate
  end function

  function brackets_strings() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    associate(scalar => string_t("do re mi"))

#if (! defined(__GFORTRAN__)) || GCC_VERSION > 150000
      associate(array  => string_t(["do", "re", "mi"]))
        test_diagnosis = test_diagnosis_t( &
          test_passed = scalar%bracket()        == string_t("[do re mi]")                                  &
               .and. all(array%bracket()        == [string_t("[do]"), string_t("[re]"), string_t("[mi]")]) &
               .and. all(array%bracket('"')     == [string_t('"do"'), string_t('"re"'), string_t('"mi"')]) &
               .and. all(array%bracket("{","}") == [string_t('{do}'), string_t('{re}'), string_t('{mi}')]) &
          ,diagnostics_string = "" &
        )
      end associate
#else
      block
        type(string_t), allocatable :: array(:)
        array = string_t(["do", "re", "mi"])
        test_diagnosis = test_diagnosis_t( &
          test_passed = scalar%bracket()        == string_t("[do re mi]")                                  &
               .and. all(array%bracket()        == [string_t("[do]"), string_t("[re]"), string_t("[mi]")]) &
               .and. all(array%bracket('"')     == [string_t('"do"'), string_t('"re"'), string_t('"mi"')]) &
               .and. all(array%bracket("{","}") == [string_t('{do}'), string_t('{re}'), string_t('{mi}')]) &
          ,diagnostics_string = "" &
        )
      end block
#endif
    end associate
  end function

  function constructs_separated_values() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis

    test_diagnosis = test_diagnosis_t( &
      test_passed = &
              "a,bc,def" == .csv. [string_t("a"), string_t("bc"), string_t("def")]    &
        .and. "abc,def"  == .csv. ["abc", "def"]                                      &
        .and. "do|re|mi" == (string_t(["do", "re", "mi"])         .sv.          "|" ) &
        .and. "dore|mi"  == (([string_t("dore"), string_t("mi")]) .sv. string_t("|")) &
        .and. "do|re|mi" == (         ["do", "re", "mi"]          .sv.          "|" ) &
        .and. "do|re|mi" == (         ["do", "re", "mi"]          .sv. string_t("|")) &
      ,diagnostics_string = "" &
    )
  end function

end module string_test_m
