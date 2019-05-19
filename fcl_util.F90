module fcl_util
  
  use fcl_kinds
  
  implicit none
  
  private
  
  public :: fcl_util_pretty_print
  
  interface fcl_util_pretty_print
    module procedure pretty_print_array_2d_int_i32
    module procedure pretty_print_array_2d_real_sp
    module procedure pretty_print_array_2d_real_dp
    module procedure pretty_print_array_2d_real_qp
  end interface ! fcl_util_pretty_print
  
contains

#define SPECIFIC_PROCEDURE pretty_print_array_2d_int_i32
#define SPECIFIC_TYPE integer
#define SPECIFIC_KIND i32
#include "fcl_util/pretty_print_array_2d.f90"
#undef SPECIFIC_PROCEDURE
#undef SPECIFIC_TYPE
#undef SPECIFIC_KIND

#define SPECIFIC_PROCEDURE pretty_print_array_2d_real_sp
#define SPECIFIC_TYPE real
#define SPECIFIC_KIND sp
#include "fcl_util/pretty_print_array_2d.f90"
#undef SPECIFIC_PROCEDURE
#undef SPECIFIC_TYPE
#undef SPECIFIC_KIND

#define SPECIFIC_PROCEDURE pretty_print_array_2d_real_dp
#define SPECIFIC_TYPE real
#define SPECIFIC_KIND dp
#include "fcl_util/pretty_print_array_2d.f90"
#undef SPECIFIC_PROCEDURE
#undef SPECIFIC_TYPE
#undef SPECIFIC_KIND

#define SPECIFIC_PROCEDURE pretty_print_array_2d_real_qp
#define SPECIFIC_TYPE real
#define SPECIFIC_KIND qp
#include "fcl_util/pretty_print_array_2d.f90"
#undef SPECIFIC_PROCEDURE
#undef SPECIFIC_TYPE
#undef SPECIFIC_KIND
  
end module fcl_util