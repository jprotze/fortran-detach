module detach
        interface
          subroutine mpix_detach (request, cb, event, ierr)
            use omp_lib_kinds
            integer :: request, ierr
            external :: cb
            integer (kind=omp_event_handle_kind), &
              value, intent(in) :: event
          end subroutine mpix_detach
        end interface
end module
program test

    use omp_lib
    use detach

    implicit none

    include "mpif.h"

    integer ::base=0, tmp_int=0, rbuf
    integer(omp_event_handle_kind) :: event
    integer :: request

    integer ::ierr, irank, isize, provided

    integer(kind = mpi_address_kind)::wsize, targ_disp

    CALL MPI_INIT_THREAD(MPI_THREAD_MULTIPLE, provided, ierr)
    IF (provided .LT. MPI_THREAD_MULTIPLE) THEN
        WRITE(*,'(A)') 'The threading support level is lesser than that demanded.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierr)
    END IF

    CALL MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)

    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)

    print *, "Hello from pe", irank, base

    !$omp parallel num_threads(2) private(ierr)
    !$omp master
    !$omp task private(request) detach(event)
    print *, "Hello from thread", omp_get_thread_num()
    call mpi_irecv(rbuf, 1, MPI_INTEGER, isize-irank-1, 42, MPI_COMM_WORLD, request, ierr)
    call mpix_detach(request, omp_fulfill_event, event, ierr)
!    call mpi_wait(request, MPI_STATUS_IGNORE, ierr)
    !$omp end task
    CALL SLEEP(1)
    call mpi_send(irank, 1, MPI_INTEGER, isize-irank-1, 42, MPI_COMM_WORLD, ierr)
    !$omp taskwait
    !$omp end master
    !$omp end parallel
    
    

    print *, "Goodbye from pe", irank, base

    CALL MPI_FINALIZE(ierr)

end program
