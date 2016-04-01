module fcl_constants

  use fcl_kinds

  implicit none

  ! SI prefixes

  real(kind=dp), parameter :: yotta = 1.0d+24
  real(kind=dp), parameter :: zetta = 1.0d+21
  real(kind=dp), parameter :: exa   = 1.0d+18
  real(kind=dp), parameter :: peta  = 1.0d+15
  real(kind=dp), parameter :: tera  = 1.0d+12
  real(kind=dp), parameter :: giga  = 1.0d+9
  real(kind=dp), parameter :: mega  = 1.0d+6
  real(kind=dp), parameter :: kilo  = 1.0d+3
  real(kind=dp), parameter :: hecto = 1.0d+2
  real(kind=dp), parameter :: deca  = 1.0d+1

  real(kind=dp), parameter :: deci  = 1.0d-1
  real(kind=dp), parameter :: centi = 1.0d-2
  real(kind=dp), parameter :: milli = 1.0d-3
  real(kind=dp), parameter :: micro = 1.0d-6
  real(kind=dp), parameter :: nano  = 1.0d-9
  real(kind=dp), parameter :: pico  = 1.0d-12
  real(kind=dp), parameter :: femto = 1.0d-15
  real(kind=dp), parameter :: atto  = 1.0d-18
  real(kind=dp), parameter :: zepto = 1.0d-21
  real(kind=dp), parameter :: yocto = 1.0d-24

  ! CODATA 2014
  real(kind=dp), parameter :: bohr_radius = 0.52917721067d-10
  real(kind=dp), parameter :: electron_mass = 9.10938356d-31
  real(kind=dp), parameter :: unified_atomic_mass_unit = 1.660539040d-27
  real(kind=dp), parameter :: planck_constant = 6.626070040d-34
  real(kind=dp), parameter :: speed_of_light_in_vacuum = 299792458._dp
  real(kind=dp), parameter :: hartree_energy = 4.359744650d-18
  real(kind=dp), parameter :: atomic_unit_of_force = 8.23872336d-8

  real(kind=dp), parameter :: pi = 3.1415926535897932_dp

end module fcl_constants
