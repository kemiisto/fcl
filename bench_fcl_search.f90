program bench_fcl_search

  use fcl_search

  implicit none

  integer, parameter :: array_size = int(1e8)
  integer, parameter :: max_value = 100
  integer, dimension(array_size) :: random_array
  integer :: i
  integer :: c
  integer :: seed_size
  integer, dimension(:), allocatable :: seed
  real :: temp
  real :: start_time, finish_time

  print *, "Benchmarking..."
  call random_seed(size = seed_size)
  allocate(seed(seed_size))
  do i = 1, seed_size
    call system_clock(count = c)
    seed(i) = c
  end do
  call random_seed(put = seed)
  deallocate(seed)

  call cpu_time(start_time)
  do i = 1, array_size
    call random_number(temp)
    random_array(i) = 1 + floor(max_value * temp)
  end do
  call cpu_time(finish_time)
  print '("Time = ", f6.3, " seconds.")', finish_time - start_time

  call cpu_time(start_time)
  print *, fcl_search_mod_linear_search(random_array, max_value + 1)
  call cpu_time(finish_time)
  print '("Time = ", f6.3, " seconds.")', finish_time - start_time

  call cpu_time(start_time)
  ! print *, findloc(random_array, max_value + 1)
  print *, maxloc(random_array)
  call cpu_time(finish_time)
  print '("Time = ", f6.3, " seconds.")', finish_time - start_time


end program bench_fcl_search