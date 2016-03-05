module fcl_lapack

  use fcl_kinds

  implicit none

  private

  public :: fcl_lapack_dsyev

  interface
    subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info)
      character        :: jobz, uplo
      integer          :: info, lda, lwork, n
      double precision :: a(lda, *), w(*), work(*)
    end subroutine dsyev
  end interface

contains

  subroutine fcl_lapack_dsyev(matrix, eigenvalues, compute_eigenvectors)
    real(kind=d), dimension(:, :), intent(inout) :: matrix
    real(kind=d), dimension(size(matrix,1)), intent(inout) :: eigenvalues
    logical, intent(in) :: compute_eigenvectors 

    ! integer, parameter :: lwmax = 1000

    integer :: n, info, lwork
    real(kind=d), dimension(1) :: work_opt_size
    real(kind=d), dimension(:), allocatable :: work
    integer :: allocation_status
    character(len=256) :: error_message
    character(len=1) :: jobz

    n = size(matrix,1)
    if (compute_eigenvectors) then
      jobz = "v"
    else
      jobz = "n"
    end if
    
    ! Query the optimal workspace.
    lwork = -1
    call dsyev(jobz, 'u', n, matrix, n, eigenvalues, work_opt_size, lwork, info)
    ! print *, "The optimal size of the WORK array:", int(work_opt_size(1))
    ! lwork = min(lwmax, int(work_opt_size(1)))
    lwork = int(work_opt_size(1))
    allocate (work(lwork), stat=allocation_status, errmsg=error_message)
    if (allocation_status > 0) then
      print *, trim(error_message)
      stop
    end if

    ! Solve the eigenproblem.
    call dsyev(jobz, 'u', n, matrix, n, eigenvalues, work, lwork, info)
    if (info < 0) then
      print *, info, "-th argument in a call to dsyev() had an illegal value"
      stop
    else if (info > 0) then
      print *, "dsyev() failed to converge"
      stop
    end if
    
    deallocate (work)
  end subroutine fcl_lapack_dsyev

end module fcl_lapack