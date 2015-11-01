module fcl_constants

  use fcl_kinds

  implicit none

  ! SI prefixes

  real(kind=d), parameter :: yotta = 1.0d+24
  real(kind=d), parameter :: zetta = 1.0d+21
  real(kind=d), parameter :: exa   = 1.0d+18
  real(kind=d), parameter :: peta  = 1.0d+15
  real(kind=d), parameter :: tera  = 1.0d+12
  real(kind=d), parameter :: giga  = 1.0d+9
  real(kind=d), parameter :: mega  = 1.0d+6
  real(kind=d), parameter :: kilo  = 1.0d+3
  real(kind=d), parameter :: hecto = 1.0d+2
  real(kind=d), parameter :: deca  = 1.0d+1

  real(kind=d), parameter :: deci  = 1.0d-1
  real(kind=d), parameter :: centi = 1.0d-2
  real(kind=d), parameter :: milli = 1.0d-3
  real(kind=d), parameter :: micro = 1.0d-6
  real(kind=d), parameter :: nano  = 1.0d-9
  real(kind=d), parameter :: pico  = 1.0d-12
  real(kind=d), parameter :: femto = 1.0d-15
  real(kind=d), parameter :: atto  = 1.0d-18
  real(kind=d), parameter :: zepto = 1.0d-21
  real(kind=d), parameter :: yocto = 1.0d-24

  ! CODATA 2014
  real(kind=d), parameter :: bohr_radius = 0.52917721067d-10
  real(kind=d), parameter :: electron_mass = 9.10938356d-31
  real(kind=d), parameter :: unified_atomic_mass_unit = 1.660539040d-27
  real(kind=d), parameter :: planck_constant = 6.626070040d-34
  real(kind=d), parameter :: speed_of_light_in_vacuum = 299792458._d
  real(kind=d), parameter :: hartree_energy = 4.359744650d-18
  real(kind=d), parameter :: atomic_unit_of_force = 8.23872336d-8

  real(kind=d), parameter :: pi = 3.1415926535897932_d

end module fcl_constants