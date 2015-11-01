module fcl_search

  implicit none

  private

  public :: fcl_search_mod_linear_search, fcl_search_mod_binary_search

  interface fcl_search_mod_linear_search
    module procedure linear_search_integer_1d
    module procedure linear_search_integer_2d
  end interface fcl_search_mod_linear_search

  interface fcl_search_mod_binary_search
    module procedure binary_search
  end interface fcl_search_mod_binary_search

contains

  ! Tries to find an element equal to value in a one-dimensional integer array.
  ! Returns:
  !   a) an index of such element counting from 1 or
  !   b) 0 if such element has not been found.
  ! If the array contains several elements equal to value,
  ! then returns the index of the first occurrence.
  pure function linear_search_integer_1d(array, value) result(i)
    integer, dimension(:), intent(in) :: array
    integer, intent(in) :: value
    integer :: i

    integer :: n

    i = 1
    n = size(array)

    do while (i <= n)
      if (array(i) == value) exit
      i = i + 1
    end do

    if (i > n) i = 0
  end function linear_search_integer_1d

  pure function linear_search_integer_2d(array, value, direction) result(indices)
    integer, dimension(:,:), intent(in) :: array
    integer, intent(in) :: value
    character(len=1), intent(in) :: direction
    integer, dimension(2) :: indices

    integer :: rows, cols

    ! assert direction is either 'c' or 'r'

    indices(1) = 1
    indices(2) = 1
    rows = size(array, 1)
    cols = size(array, 2)

    if (direction == 'c') then
      j: do while (indices(2) <= cols)
        indices(1) = 1
        i: do while (indices(1) <= rows)
          if (array(indices(1), indices(2)) == value) exit j
          indices(1) = indices(1) + 1
        end do i
        indices(2) = indices(2) + 1
      end do j
      if (indices(2) > cols) indices = 0
    else if (direction == 'r') then
      m: do while (indices(1) <= rows)
        indices(2) = 1
        n: do while (indices(2) <= cols)
          if (array(indices(1), indices(2)) == value) exit m
          indices(2) = indices(2) + 1
        end do n
        indices(1) = indices(1) + 1
      end do m
      if (indices(1) > rows) indices = 0
    end if
  end function linear_search_integer_2d

  pure function binary_search(array, value) result(r)
    integer, dimension(:), intent(in) :: array
    integer, intent(in) :: value
    integer :: m

    integer :: n, l, r

    n = size(array)
    l = 1
    r = n
    do while (l < r)
      m = (l + r) / 2
      if (array(m) < value) then
        l = m + 1
      else
        r = m
      end if
    end do

    if (array(r) /= value) r = 0
  end function binary_search

end module fcl_search
