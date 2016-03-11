subroutine SPECIFIC_PROCEDURE(unit, array, format, max_columns, headers, decorate)
  integer, intent(in) :: unit
  real(kind=REALKIND), dimension(:,:), intent(in) :: array
  character(len=*), intent(in) :: format
  integer, intent(in) :: max_columns
  logical, intent(in) :: headers, decorate
  
  integer :: rows, columns, left_column, right_column, row, i, row_header_width
  real :: format_width
  character(len=80) :: row_format, header_format, rows_string, temp
  
  rows = size(array, 1)
  columns = size(array, 2)
  
  write (rows_string,"(i80)") rows
  row_header_width = len(trim(adjustl(rows_string)))
  
  read (format(2:3),*) format_width

  write (row_format, "(i2,a,a)") max_columns, format, ")"
  write (header_format, "(i2,a,i2,a)") max_columns, "i", int(format_width), ")"

  if (decorate .or. headers) then
    row_format = "a2,"//trim(row_format)
  endif

  if (headers) then
    write (temp, "(a,i2)") "i", row_header_width
    row_format = trim(temp)//trim(row_format)
    header_format = "a,"//trim(header_format)
  endif

  row_format = "("//trim(row_format)
  header_format = "("//trim(header_format)

  ! print *, "header format:", header_format
  ! print *, "row format:", row_format
  
  left_column = 1
  right_column = max_columns
  if (right_column > columns) right_column = columns

  do while (left_column <= columns)
    if (headers) then
      write (unit, header_format) repeat(" ", row_header_width + 2), (i, i = left_column, right_column)
      if (decorate) then
        write (unit, "(a)") repeat(" ", row_header_width + 2)//repeat("-", int(format_width) * (right_column - left_column + 1))
      else
        write (unit, "(bn)")
      end if
    else
      if (decorate) then
        write (unit, "(a)") "  "//repeat("-", int(format_width) * (right_column - left_column + 1))
      end if
    end if

    do row = 1, rows
      if (headers) then
        if (decorate) then
          write (unit, row_format) row, " |", array(row, left_column:right_column)
        else
          write (unit, row_format) row, "  ", array(row, left_column:right_column)
        end if
      else
        if (decorate) then
          write (unit, row_format) "|", array(row, left_column:right_column)
        else
          write (unit, row_format) array(row, left_column:right_column)
        end if
      endif
    end do
    write (unit, "(bn)")

    left_column = left_column + max_columns
    right_column = right_column + max_columns
    if (right_column > columns) right_column = columns
  end do
end subroutine SPECIFIC_PROCEDURE