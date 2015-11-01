program test_fcl_search

  use fcl_search
  use fcl_test

  implicit none

  print *, "Testing..."

  print *, ""
  print *, "fcl_search_mod_linear_search() on 1d integer array"
  call test_fcl_search_mod_linear_search_integer_1d()

  print *, ""
  print *, "fcl_search_mod_linear_search() on 2d integer array in row order"
  call test_fcl_search_mod_linear_search_integer_2d_row_order()

  print *, ""
  print *, "fcl_search_mod_linear_search() on 2d integer array in column order"
  call test_fcl_search_mod_linear_search_integer_2d_column_order()

  print *, ""
  print *, "fcl_search_mod_binary_search()"
  call test_fcl_search_mod_binary_search()

contains

  subroutine test_fcl_search_mod_linear_search_integer_1d()
    integer, dimension(10) :: array

    array = [8, 7, 0, 6, 4, 0, 5, 5, 7, 9]
    ! non-existing elements 1, 2, 3
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  1),  0, "01" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  2),  0, "02" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  3),  0, "03" )

    ! existing unique elements 4, 6, 8, 9
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  4),  5, "04" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  6),  4, "05" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  8),  1, "06" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  9), 10, "07" )

    ! existing non-unique elements 0, 5, 7
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  0),  3, "08" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  5),  7, "09" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  7),  2, "10" )
  end subroutine test_fcl_search_mod_linear_search_integer_1d

  subroutine test_fcl_search_mod_linear_search_integer_2d_row_order()
    integer, dimension(2, 5) :: array
    !       8           0           4           5           7
    !       7           6           0           5           9
    array = reshape([8, 7, 0, 6, 4, 0, 5, 5, 7, 9], [2, 5])
    ! non-existing elements 1, 2, 3
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  1, 'r'),  [0, 0], "01" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  2, 'r'),  [0, 0], "02" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  3, 'r'),  [0, 0], "03" )

    ! existing unique elements 4, 6, 8, 9
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  4, 'r'),  [1, 3], "04" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  6, 'r'),  [2, 2], "05" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  8, 'r'),  [1, 1], "06" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  9, 'r'),  [2, 5], "07" )

    ! existing non-unique elements 0, 5, 7
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  0, 'r'),  [1, 2], "08" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  5, 'r'),  [1, 4], "09" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  7, 'r'),  [1, 5], "10" )
  end subroutine test_fcl_search_mod_linear_search_integer_2d_row_order

  subroutine test_fcl_search_mod_linear_search_integer_2d_column_order()
    integer, dimension(2, 5) :: array
    !       8           0           4           5           7
    !       7           6           0           5           9
    array = reshape([8, 7, 0, 6, 4, 0, 5, 5, 7, 9], [2, 5])
    ! non-existing elements 1, 2, 3
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  1, 'c'),  [0, 0], "01" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  2, 'c'),  [0, 0], "02" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  3, 'c'),  [0, 0], "03" )

    ! existing unique elements 4, 6, 8, 9
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  4, 'c'),  [1, 3], "04" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  6, 'c'),  [2, 2], "05" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  8, 'c'),  [1, 1], "06" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  9, 'c'),  [2, 5], "07" )

    ! existing non-unique elements 0, 5, 7
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  0, 'c'),  [1, 2], "08" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  5, 'c'),  [1, 4], "09" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  7, 'c'),  [2, 1], "10" )
  end subroutine test_fcl_search_mod_linear_search_integer_2d_column_order

  subroutine test_fcl_search_mod_binary_search()
    integer, dimension(10) :: array

    array = [0, 0, 4, 5, 5, 6, 7, 7, 8, 9]
    ! non-existing elements 1, 2, 3
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  1),  0, "01" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  2),  0, "02" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  3),  0, "03" )

    ! existing unique elements 4, 6, 8, 9
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  4),  3, "04" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  6),  6, "05" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  8),  9, "06" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  9), 10, "07" )

    ! existing non-unique elements 0, 5, 7
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  0),  1, "08" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  5),  4, "09" )
    call fcl_test_mod_assert_equal( fcl_search_mod_linear_search(array,  7),  7, "10" )
  end subroutine test_fcl_search_mod_binary_search

end program test_fcl_search
