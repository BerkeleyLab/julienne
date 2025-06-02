program create_markdown_table
  !! This program demonstrates 
  !! 1. The use of the string_t constructor function to encapsulate a ragged-edged array of character values.
  !! 2. The use of the separated-values operator(.sv.) to form one string by concatenating the elements of a
  !!   string_t array and adding user-designated separators between each resulting substring.
  !! 3. The use of the elemental type-bound procedure "bracket" to add preceding and trailing delimiters to all
  !!    elements of a string_t array.
  !! Running the program with a command of the form "fpm run --example create-markdown-table" without quotes
  !! should produce a table similar to the following with "flang" replaced by the name of the compiler if a
  !! different compiler is used:
  !! |compiler \ kind|default|c_size_t|c_int64_t|c_intptr_t|
  !! |-|-|-|-|-|
  !! |flang|4|8|8|8|
  use iso_fortran_env, only : compiler_version
  use iso_c_binding, only : c_size_t, c_int64_t, c_intptr_t
  use julienne_string_m, only : string_t, operator(.sv.)
  implicit none

  block
    integer row
    integer, parameter :: default_integer_kind = kind(0)
    integer, parameter :: body(*,*) =  reshape([default_integer_kind, c_size_t , c_int64_t , c_intptr_t], [1,4])
    type(string_t), allocatable :: table_lines(:), header(:)

    header = &
     [string_t("compiler \ kind"), string_t("default"), string_t("c_size_t"), string_t("c_int64_t"), string_t("c_intptr_t")]

    table_lines = markdown_table(row_header=[compiler()], column_header=header, body_cells=string_t(body), side_borders=.true.)

    do row = 1, size(table_lines)
      print '(a)', table_lines(row)%string()
    end do
  end block

contains

  pure function markdown_table(row_header, column_header, body_cells, side_borders) result(lines)
    integer, parameter :: first_body_row = 3
    type(string_t), intent(in) :: row_header(first_body_row:), column_header(:), body_cells(first_body_row:,:)
    logical, intent(in) :: side_borders
    character(len=1), parameter :: column_separator = "|"
    integer, parameter :: num_rule_lines = 1
    type(string_t) lines(size(body_cells,1) + rank(column_header) + num_rule_lines)
    integer row, col

    if (size(column_header) /= rank(row_header) + size(body_cells,2)) error stop "column size mismatch"
    if (size(row_header) /= size(body_cells,1)) error stop "row size mismatch"

    lines(1) = column_header .sv. column_separator
    lines(2) = [("-", col=1,size(column_header))] .sv. column_separator

    do row = 3, size(lines)
      lines(row) = [row_header(row), body_cells(row,:)] .sv. column_separator
    end do

    if (side_borders) lines = lines%bracket(column_separator)

  end function

  pure function compiler()
    type(string_t) compiler
    associate(compiler_identity => compiler_version())
      if (index(compiler_identity, "GCC") /= 0) then
        compiler = string_t("gfortran")
      else if (index(compiler_identity, "NAG") /= 0) then
        compiler = string_t("nagfor")
      else if (index(compiler_identity, "flang") /= 0) then
        compiler = string_t("flang")
      else if (index(compiler_identity, "Intel") /= 0) then
        compiler = string_t("ifx")
      else
#if (! defined(__GFORTRAN__)) || (GCC_VERSION > 140000)
          error stop "unrecognized compiler: " // compiler_identity
#else
          error stop "unrecognized compiler"
#endif
      end if
    end associate
  end function

end program
