subroutine SPECIFIC_PROCEDURE(array, fmt, max_columns, decorate)
  real(kind=REALKIND), dimension(:,:), intent(in) :: array
  character(len=*), intent(in) :: fmt
  integer, intent(in) :: max_columns
  logical, intent(in) :: decorate
  
  integer :: rows, columns, left_column, right_column, row, i, fmt_width, row_decoration_width
  character(len=80) :: row_fmt, header_format, rows_string
  
  rows = size(array, 1)
  columns = size(array, 2)
  
  write (rows_string,"(i80)") rows
  row_decoration_width = len(trim(adjustl(rows_string)))
  
  read (fmt(2:2),"(i1)") fmt_width
  if (decorate) then
    write (row_fmt, "(a,i2,a,i2,a,a)") "(i", row_decoration_width, ",a2,", max_columns, fmt, ")"
    write (header_format, "(a,i2,a,i2,a)") "(a,", max_columns, "i", fmt_width, ")"
  else
    write (row_fmt, "(a,i2,a,a)") "(", max_columns, fmt, ")"
    write (header_format, "(a,i2,a,i2,a)") "(", max_columns, "i", fmt_width, ")"
  end if
  
  left_column = 1
  right_column = max_columns
  do while (left_column <= columns)
    if (decorate) then 
      print header_format, repeat(" ", row_decoration_width + 2), (i, i = left_column, right_column)
      print "(a)", repeat(" ", row_decoration_width + 2)//repeat("-", fmt_width * (right_column - left_column + 1))
    end if
    do row = 1, rows
      if (decorate) then
        print row_fmt, row, " |", array(row, left_column:right_column)
      else
        print row_fmt, array(row, left_column:right_column)
      end if
    end do
    print *, ""
    left_column = left_column + max_columns
    right_column = right_column + max_columns
    if (right_column > columns) right_column = columns
  end do
end subroutine SPECIFIC_PROCEDURE