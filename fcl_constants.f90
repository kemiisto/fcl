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

  real(kind=d), parameter :: pi = 3.1415926535897932_d

end module fcl_constants