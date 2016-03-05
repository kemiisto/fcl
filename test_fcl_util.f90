program test_fcl_util

  use fcl_kinds
  use fcl_util

  implicit none
  
  real(kind=d), dimension(8, 9) :: array
  character(len=*), parameter :: fmt = "f5.1"
  
  integer :: rows, columns, i, j
  
  rows = size(array, 1)
  columns = size(array, 2)
  
!   array = transpose(           &
!     reshape(                   &
!       [                        &
!         1.1_d, 1.2_d, 1.3_d,   &
!         2.1_d, 2.2_d, 2.3_d    &
!       ],                       &
!       [columns, rows]          &
!     )                          &
!   )
  
  do j = 1, columns
    do i = 1, rows
      array(i, j) = i + j * 0.1_d
    end do
  end do
  
  call fcl_util_pretty_print(array, fmt, columns, .true.)
  call fcl_util_pretty_print(array, fmt, columns, .false.)
  call fcl_util_pretty_print(array, fmt, 5, .true.)
  call fcl_util_pretty_print(array, fmt, 5, .false.)

end program test_fcl_util
