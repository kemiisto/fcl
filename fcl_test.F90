module fcl_test

  use fcl_kinds

  implicit none

  private

  public :: fcl_test_mod_assert_equal, fcl_test_mod_assert_equal_real

  interface fcl_test_mod_assert_equal
    module procedure fcl_test_mod_assert_equal_integer
    module procedure fcl_test_mod_assert_equal_logical
  end interface fcl_test_mod_assert_equal

  interface fcl_test_mod_assert_equal_real
    module procedure fcl_test_mod_assert_equal_real_sp
    module procedure fcl_test_mod_assert_equal_real_dp
    module procedure fcl_test_mod_assert_equal_real_qp
  end interface fcl_test_mod_assert_equal_real

contains

! ------------------------------------------------------------------------------

#define SPECIFIC_OPERATION actual == expected

#define SPECIFIC_PROCEDURE fcl_test_mod_assert_equal_integer
#define SPECIFIC_TYPE integer
#include "fcl_test/assert_equal.f90"
#undef SPECIFIC_PROCEDURE
#undef SPECIFIC_TYPE

#undef SPECIFIC_OPERATION

! ------------------------------------------------------------------------------

#define SPECIFIC_OPERATION actual .eqv. expected

#define SPECIFIC_PROCEDURE fcl_test_mod_assert_equal_logical
#define SPECIFIC_TYPE logical
#include "fcl_test/assert_equal.f90"
#undef SPECIFIC_PROCEDURE
#undef SPECIFIC_TYPE

#undef SPECIFIC_OPERATION

! ------------------------------------------------------------------------------

#define SPECIFIC_PROCEDURE fcl_test_mod_assert_equal_real_sp
#define REALKIND sp
#include "fcl_test/assert_equal_real.f90"
#undef SPECIFIC_PROCEDURE
#undef REALKIND

#define SPECIFIC_PROCEDURE fcl_test_mod_assert_equal_real_dp
#define REALKIND dp
#include "fcl_test/assert_equal_real.f90"
#undef SPECIFIC_PROCEDURE
#undef REALKIND

#define SPECIFIC_PROCEDURE fcl_test_mod_assert_equal_real_qp
#define REALKIND qp
#include "fcl_test/assert_equal_real.f90"
#undef SPECIFIC_PROCEDURE
#undef REALKIND

  subroutine print_passed_message(description)
    character(len=*), intent(in) :: description

    print '(a, a10, a)', "|- ", description, " passed."
  end subroutine print_passed_message

  subroutine print_failed_message(description)
    character(len=*), intent(in) :: description

    print '(a, a10, a)', "|- ", description, " FAILED!"
  end subroutine print_failed_message

end module fcl_test
