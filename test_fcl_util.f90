program test_fcl_util

  use iso_fortran_env
  use fcl_kinds
  use fcl_util

  implicit none

  integer, parameter :: rows = 3
  integer, parameter :: cols = 4

  character(len=*), parameter :: fmt_int = "i5"
  character(len=*), parameter :: fmt_real = "f5.1"
  
  integer, dimension(rows, cols) :: array_2d_int_def
  real(kind=dp), dimension(rows, cols) :: array_2d_real_dp
  
  integer :: i, j
  
  do j = 1, cols
    do i = 1, rows
      array_2d_int_def(i, j) = i * 10 + j
      array_2d_real_dp(i, j) = i + j * 0.1_dp
    end do
  end do

  call fcl_util_pretty_print(output_unit, array_2d_int_def, fmt_int, cols, headers=.true., decorate=.true.)
  call fcl_util_pretty_print(output_unit, array_2d_int_def, fmt_int, cols, headers=.true., decorate=.false.)
  call fcl_util_pretty_print(output_unit, array_2d_int_def, fmt_int, cols, headers=.false., decorate=.true.)
  call fcl_util_pretty_print(output_unit, array_2d_int_def, fmt_int, cols, headers=.false., decorate=.false.)
  
  call fcl_util_pretty_print(output_unit, array_2d_real_dp, fmt_real, cols, headers=.true., decorate=.true.)
  call fcl_util_pretty_print(output_unit, array_2d_real_dp, fmt_real, cols, headers=.true., decorate=.false.)
  call fcl_util_pretty_print(output_unit, array_2d_real_dp, fmt_real, cols, headers=.false., decorate=.true.)
  call fcl_util_pretty_print(output_unit, array_2d_real_dp, fmt_real, cols, headers=.false., decorate=.false.)

  ! call fcl_util_pretty_print(output_unit, array, fmt, 5, headers=.true., decorate=.true.)
  ! call fcl_util_pretty_print(output_unit, array, fmt, 5, headers=.true., decorate=.false.)

end program test_fcl_util
