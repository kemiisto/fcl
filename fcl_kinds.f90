module fcl_kinds

  ! Intel implementation of ieee_selected_real_kind() is buggy.
  ! https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/293623
  !
  ! use, intrinsic :: ieee_arithmetic

  implicit none

  ! single-precision floating-point format:
  !   "single" in ieee 754-1985, "binary32" in ieee 754-2008; 
  !   occupies 4 bytes (32 bits) in computer memory;
  !   6 to 9 significant decimal digits precision;
  !   38 approximate exponent range.
  ! integer, parameter :: sp = ieee_selected_real_kind(6, 37)
  integer, parameter :: sp = selected_real_kind(6, 37)
  real(kind=sp), parameter :: eps_sp = epsilon(1.0_sp)

  ! double-precision floating-point format:
  !   "double" in ieee 754-1985, "binary64" in ieee 754-2008;
  !   occupies 8 bytes (64 bits) in computer memory;
  !   15-17 significant decimal digits precision;
  !   308 approximate exponent range.
  ! integer, parameter :: dp = ieee_selected_real_kind(15, 307)
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(kind=dp), parameter :: eps_d = epsilon(1.0_dp)

  integer, parameter :: qp = selected_real_kind(33, 4931)
  real(kind=qp), parameter :: eps_qp = epsilon(1.0_qp)
  
end module fcl_kinds
