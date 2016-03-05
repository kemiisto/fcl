module fcl_util
  
  use fcl_kinds
  
  implicit none
  
  private
  
  public :: fcl_util_pretty_print
  
  interface fcl_util_pretty_print
    module procedure pretty_print_array_2d_real_s
    module procedure pretty_print_array_2d_real_d
  end interface ! fcl_util_pretty_print
  
contains

#define SPECIFIC_PROCEDURE pretty_print_array_2d_real_s
#define REALKIND s
#include "fcl_util/pretty_print_array_2d_real.f90"
#undef SPECIFIC_PROCEDURE
#undef REALKIND

#define SPECIFIC_PROCEDURE pretty_print_array_2d_real_d
#define REALKIND d
#include "fcl_util/pretty_print_array_2d_real.f90"
#undef SPECIFIC_PROCEDURE
#undef REALKIND
  
end module fcl_util