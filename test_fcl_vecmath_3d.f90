program test_fcl_vecmath_3d

  use fcl_kinds
  use fcl_vecmath_vector_3d
  use fcl_test

  implicit none

  real(kind=d), parameter :: tolerance = 0.001_d

  type(fcl_vecmath_mod_vector3d) :: zero, &
    e_i, e_j, e_k,                    &
    minus_e_i, minus_e_j, minus_e_k,  &
    v

        zero = fcl_vecmath_mod_vector3d( [ 0.0_d,  0.0_d,  0.0_d] )
         e_i = fcl_vecmath_mod_vector3d( [ 1.0_d,  0.0_d,  0.0_d] )
         e_j = fcl_vecmath_mod_vector3d( [ 0.0_d,  1.0_d,  0.0_d] )
         e_k = fcl_vecmath_mod_vector3d( [ 0.0_d,  0.0_d,  1.0_d] )
   minus_e_i = fcl_vecmath_mod_vector3d( [-1.0_d,  0.0_d,  0.0_d] )
   minus_e_j = fcl_vecmath_mod_vector3d( [ 0.0_d, -1.0_d,  0.0_d] )
   minus_e_k = fcl_vecmath_mod_vector3d( [ 0.0_d,  0.0_d, -1.0_d] )
           v = fcl_vecmath_mod_vector3d( [ 2.0_d,  3.0_d,  6.0_d] )

  print *, "Testing..."

  print *, ""
  print *, "vector3d_norm()"
  call test_vector3d_norm()

  print *, ""
  print *, "vector3d_is_equal()"
  call test_vector3d_is_equal()

  print *, ""
  print *, "vector3d_plus_vector3d()"
  call test_vector3d_plus_vector3d()
  
contains

  subroutine test_vector3d_norm()
    call fcl_test_mod_assert_equal_real( &
      zero%norm(), 0.0_d, tolerance, "zero" &
    )
    call fcl_test_mod_assert_equal_real( &
      e_i%norm(), 1.0_d, tolerance, "x" &
    )
    call fcl_test_mod_assert_equal_real( &
      e_j%norm(), 1.0_d, tolerance, "y" &
    )
    call fcl_test_mod_assert_equal_real( &
      e_k%norm(), 1.0_d, tolerance, "z" &
    )
    call fcl_test_mod_assert_equal_real( &
      minus_e_i%norm(), 1.0_d, tolerance,   "-x" &
    )
    call fcl_test_mod_assert_equal_real( &
      minus_e_j%norm(), 1.0_d, tolerance,   "-y" &
    )
    call fcl_test_mod_assert_equal_real( &
      minus_e_k%norm(), 1.0_d, tolerance,   "-z" &
    )
    call fcl_test_mod_assert_equal_real( &
      v%norm(), 7.0_d, tolerance, "xyz" &
    )
  end subroutine test_vector3d_norm


  subroutine test_vector3d_is_equal()
    call fcl_test_mod_assert_equal( &
      zero%is_equal(zero, tolerance), .true., "zero" &
    )
    call fcl_test_mod_assert_equal( &
      e_i%is_equal(e_i, tolerance), .true., "x" &
    )
    call fcl_test_mod_assert_equal( &
      e_j%is_equal(e_j, tolerance), .true., "y" &
    )
    call fcl_test_mod_assert_equal( &
      e_k%is_equal(e_k, tolerance), .true., "z" &
    )
    call fcl_test_mod_assert_equal( &
      v%is_equal(v, tolerance), .true., "xyz" &
    )
    call fcl_test_mod_assert_equal( &
      zero%is_equal(e_i, tolerance), .false., "false" &
    )
  end subroutine test_vector3d_is_equal


  subroutine test_vector3d_plus_vector3d()
    type(fcl_vecmath_mod_vector3d) :: result

    result = zero + zero
    call fcl_test_mod_assert_equal( &
      result%is_equal(zero, tolerance), .true., "zero" &
    )

    result = e_i + e_i
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([2.0_d, 0.0_d, 0.0_d]), tolerance), .true., "x" &
    )

    result = e_j + e_j
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([0.0_d, 2.0_d, 0.0_d]), tolerance), .true., "y" &
    )

    result = e_k + e_k
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([0.0_d, 0.0_d, 2.0_d]), tolerance), .true., "z" &
    )

    result = e_i + e_j
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([1.0_d, 1.0_d, 0.0_d]), tolerance), .true., "xy" &
    )

    result = e_i + e_k
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([1.0_d, 0.0_d, 1.0_d]), tolerance), .true., "xz" &
    )

    result = e_j + e_k
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([0.0_d, 1.0_d, 1.0_d]), tolerance), .true., "yz" &
    )

    result = v + v
    call fcl_test_mod_assert_equal( &
      result%is_equal(fcl_vecmath_mod_vector3d([4.0_d, 6.0_d, 12.0_d]), tolerance), .true., "xyz" &
    )
  end subroutine test_vector3d_plus_vector3d

end program test_fcl_vecmath_3d
