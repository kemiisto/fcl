  use fcl_kinds

  implicit none

  private

  public :: TYPE_NAME, &
    operator(+), operator(-), & 
    operator(*), operator(/), &
    operator(.dot.) &
#if VECTOR_DIMENSION == 3
    , operator(.cross.)
#endif

  type :: TYPE_NAME
    COMPONENTS_TYPE(kind=COMPONENTS_KIND), dimension(VECTOR_DIMENSION) :: data
  contains
#if VECTOR_DIMENSION == 3
    procedure :: x          => fcl_vecmath_mod_vector_x
    procedure :: y          => fcl_vecmath_mod_vector_y
    procedure :: z          => fcl_vecmath_mod_vector_z
#endif
#if COMPONENTS_TYPE == real
    procedure :: angle      => fcl_vecmath_mod_vector_angle
#endif
    procedure :: is_equal   => fcl_vecmath_mod_vector_is_equal
    procedure :: is_zero    => fcl_vecmath_mod_vector_is_zero
    procedure :: norm       => fcl_vecmath_mod_vector_norm
    procedure :: normalized => fcl_vecmath_mod_vector_normalized
  end type TYPE_NAME

  interface operator(+)
    module procedure fcl_vecmath_mod_vector_plus_vector
  end interface

  interface operator(-)
    module procedure fcl_vecmath_mod_vector_minus_vector
    module procedure fcl_vecmath_mod_vector_negative
  end interface

  interface operator(*)
    module procedure fcl_vecmath_mod_vector_times_real
  end interface

  interface operator(/)
    module procedure fcl_vecmath_mod_vector_divided_by_real
  end interface

  interface operator(.dot.)
    module procedure fcl_vecmath_mod_vector_dot_vector
  end interface

#if VECTOR_DIMENSION == 3
  interface operator(.cross.)
    module procedure fcl_vecmath_mod_vector_cross_vector
  end interface
#endif

#if COMPONENTS_TYPE == real
  real(kind=COMPONENTS_KIND), parameter :: zero = real(0, kind=COMPONENTS_KIND)
  real(kind=COMPONENTS_KIND), parameter :: one  = real(1, kind=COMPONENTS_KIND)
#endif

  type(TYPE_NAME), parameter :: fcl_vecmath_mod_vector_zero = &
    TYPE_NAME(COMPONENTS_TYPE(0, kind=COMPONENTS_KIND))

contains

  ! Angle (in radians) between this vector and the other vector.
  !
  ! cos(phi) = (v_ji . v_jk) / (  )
  pure function fcl_vecmath_mod_vector_angle(this, other) result(f)
    class(TYPE_NAME), intent(in) :: this
    type(TYPE_NAME), intent(in) :: other
    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: f

    real(kind=COMPONENTS_KIND) :: cos_phi

    ! avoid division by zero for the zero vector 
    if (this%is_zero() .or. other%is_zero()) then
      cos_phi = zero
    else
      cos_phi = (this .dot. other) / (this%norm() * other%norm())
      if (cos_phi < -one) then
        cos_phi = -one
      else if (cos_phi > one) then
        cos_phi = one
      end if
    end if

    f = acos(cos_phi)
  end function fcl_vecmath_mod_vector_angle


  pure function fcl_vecmath_mod_vector_is_equal(this, other, tolerance) result(f)
    class(TYPE_NAME), intent(in) :: this
    type(TYPE_NAME), intent(in) :: other
    COMPONENTS_TYPE(kind=COMPONENTS_KIND), intent(in), optional :: tolerance
    logical :: f

    type(TYPE_NAME) :: diff

    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: min_norm, tol

    if (present(tolerance)) then
      tol = tolerance
    else
      tol = eps_d
    end if

    diff = this - other
    
    if (this%norm() <= other%norm()) then
      min_norm = this%norm()
    else
      min_norm = other%norm()
    end if

    f = diff%norm() <= tol * min_norm
  end function fcl_vecmath_mod_vector_is_equal


  pure function fcl_vecmath_mod_vector_is_zero(this, tolerance) result(f)
    class(TYPE_NAME), intent(in) :: this
    COMPONENTS_TYPE(kind=COMPONENTS_KIND), intent(in), optional :: tolerance
    logical :: f

    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: tol

    if (present(tolerance)) then
      tol = tolerance
    else
      tol = eps_d
    end if

    f = this%is_equal(fcl_vecmath_mod_vector_zero, tol)
  end function fcl_vecmath_mod_vector_is_zero


  pure function fcl_vecmath_mod_vector_norm(this) result(f)
    class(TYPE_NAME), intent(in) :: this
    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: f

    f = sqrt(this .dot. this)
  end function fcl_vecmath_mod_vector_norm


  pure function fcl_vecmath_mod_vector_normalized(this) result(f)
    class(TYPE_NAME), intent(in) :: this
    type(TYPE_NAME) :: f

    f = this / this%norm()
  end function fcl_vecmath_mod_vector_normalized


  pure function fcl_vecmath_mod_vector_plus_vector(v1, v2) result(f)
    type(TYPE_NAME), intent(in) :: v1, v2
    type(TYPE_NAME) :: f

    f%data = v1%data + v2%data
  end function fcl_vecmath_mod_vector_plus_vector


  pure function fcl_vecmath_mod_vector_minus_vector(v1, v2) result(f)
    type(TYPE_NAME), intent(in) :: v1, v2
    type(TYPE_NAME) :: f

    f%data = v1%data - v2%data
  end function fcl_vecmath_mod_vector_minus_vector


  pure function fcl_vecmath_mod_vector_negative(v) result(f)
    type(TYPE_NAME), intent(in) :: v
    type(TYPE_NAME) :: f

    f%data = -v%data
  end function fcl_vecmath_mod_vector_negative


  pure function fcl_vecmath_mod_vector_times_real(v, r) result(f)
    type(TYPE_NAME), intent(in) :: v
    COMPONENTS_TYPE(kind=COMPONENTS_KIND), intent(in) :: r
    type(TYPE_NAME) :: f

    f%data = v%data * r
  end function fcl_vecmath_mod_vector_times_real


  pure function fcl_vecmath_mod_vector_divided_by_real(v, r) result(f)
    type(TYPE_NAME), intent(in) :: v
    COMPONENTS_TYPE(kind=COMPONENTS_KIND), intent(in) :: r
    type(TYPE_NAME) :: f

    f%data = v%data / r
  end function fcl_vecmath_mod_vector_divided_by_real


  pure function fcl_vecmath_mod_vector_dot_vector(v1, v2) result(f)
    type(TYPE_NAME), intent(in) :: v1, v2
    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: f

    f = dot_product(v1%data, v2%data)
  end function fcl_vecmath_mod_vector_dot_vector


#if VECTOR_DIMENSION == 3
  pure function fcl_vecmath_mod_vector_x(this) result(res)
    class(TYPE_NAME), intent(in) :: this
    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: res

    res = this%data(1)
  end function fcl_vecmath_mod_vector_x


  pure function fcl_vecmath_mod_vector_y(this) result(res)
    class(TYPE_NAME), intent(in) :: this
    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: res

    res = this%data(2)
  end function fcl_vecmath_mod_vector_y


  pure function fcl_vecmath_mod_vector_z(this) result(res)
    class(TYPE_NAME), intent(in) :: this
    COMPONENTS_TYPE(kind=COMPONENTS_KIND) :: res

    res = this%data(3)
  end function fcl_vecmath_mod_vector_z

  
  pure function fcl_vecmath_mod_vector_cross_vector(v1, v2) result(f)
    type(TYPE_NAME), intent(in) :: v1, v2
    type(TYPE_NAME) :: f

    f%data(1) = v1%data(2) * v2%data(3) - v1%data(3) * v2%data(2)
    f%data(2) = v1%data(3) * v2%data(1) - v1%data(1) * v2%data(3)
    f%data(3) = v1%data(1) * v2%data(2) - v1%data(2) * v2%data(1)
  end function fcl_vecmath_mod_vector_cross_vector
#endif