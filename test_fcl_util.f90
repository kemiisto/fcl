program test_fcl_util

  use iso_fortran_env
  use fcl_kinds
  use fcl_util

  implicit none
  
  real(kind=d), dimension(3, 4) :: array
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
  
  call fcl_util_pretty_print(output_unit, array, fmt, columns, headers=.true., decorate=.true.)
  call fcl_util_pretty_print(output_unit, array, fmt, columns, headers=.true., decorate=.false.)

  call fcl_util_pretty_print(output_unit, array, fmt, columns, headers=.false., decorate=.true.)
  call fcl_util_pretty_print(output_unit, array, fmt, columns, headers=.false., decorate=.false.)

  ! call fcl_util_pretty_print(output_unit, array, fmt, 5, headers=.true., decorate=.true.)
  ! call fcl_util_pretty_print(output_unit, array, fmt, 5, headers=.true., decorate=.false.)

end program test_fcl_util
