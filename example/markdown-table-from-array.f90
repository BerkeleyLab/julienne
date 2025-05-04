program build_markdown_table_from_array
  !! Build a GitHub Markdown table from an array
  use julienne_string_m, only : string_t, markdown_table
  implicit none

  integer row, col
  integer, parameter :: rows = 3, cols = 4 
  integer, parameter :: integer_array(*,*) = reshape([([(row*col, row=1,rows)], col=1,cols)], [rows,cols])
  real, parameter :: real_array(*,*) = reshape([([(real(row*col), row=1,rows)], col=1,cols)], [rows,cols])
  type(string_t), allocatable :: table_lines(:)

  table_lines = markdown_table(string_t(integer_array), side_borders=.true.)

  do row = 1, size(table_lines)
    print '(a)', table_lines(row)%string()
  end do

  table_lines = markdown_table(string_t(real_array), side_borders=.false.)

  do row = 1, size(table_lines)
    print '(a)', table_lines(row)%string()
  end do

end program
