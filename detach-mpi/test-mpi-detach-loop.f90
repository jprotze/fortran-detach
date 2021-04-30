module mpi_detach
    interface
      subroutine mpix_detach (request, cb, event, ierr)
        use omp_lib_kinds
        integer :: request, ierr
        external :: cb
        integer (kind=omp_event_handle_kind), &
          value, intent(in) :: event
      end subroutine mpix_detach
      subroutine mpix_detach_all (count, requests, cb, event, ierr)
        use omp_lib_kinds
        integer :: requests(*)
        integer :: count, ierr
        external :: cb
        integer (kind=omp_event_handle_kind), &
          value, intent(in) :: event
      end subroutine mpix_detach_all
      subroutine mpix_detach_task (request, event, ierr)
        use omp_lib_kinds
        integer :: request, ierr
        integer (kind=omp_event_handle_kind), &
          value, intent(in) :: event
      end subroutine mpix_detach_task
      subroutine mpix_detach_all_task (count, requests, event, ierr)
        use omp_lib_kinds
        integer :: requests(*)
        integer :: count, ierr
        integer (kind=omp_event_handle_kind), &
          value, intent(in) :: event
      end subroutine mpix_detach_all_task
    end interface
end module
program test

    use omp_lib
    !use mpi_detach

    implicit none

    include "mpif.h"

    integer ::base=0, tmp_int=0, rbuf, i, j
    integer(omp_event_handle_kind) :: oevent, ievent
    integer :: request, comm(0:4)

    integer ::ierr, irank, isize, provided

    integer(kind = mpi_address_kind)::wsize, targ_disp
    integer :: A(0:4)
    integer :: B(0:4)
    A = (/1,2,3,4,5/)
    B = -1

    CALL MPI_INIT_THREAD(MPI_THREAD_MULTIPLE, provided, ierr)
    IF (provided .LT. MPI_THREAD_MULTIPLE) THEN
        WRITE(*,'(A)') 'The threading support level is lesser than that demanded.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierr)
    END IF


    CALL MPI_COMM_RANK(MPI_COMM_WORLD, irank, ierr)

    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, isize, ierr)
    DO i=0,4
      CALL MPI_COMM_DUP(MPI_COMM_WORLD, comm(i), ierr)
    ENDDO

    print *, "Hello from pe", irank, base

    !$omp parallel num_threads(3) private(ierr)
    !$omp single
    DO i=0,14
      j = modulo(i,5)
    !$omp task detach(oevent) depend(inout:A(j)) firstprivate(j, ievent)
          print *, "Outer Task j=", j
		!$omp task detach(ievent) depend(inout:A(j))
		  print *, "Inner Task1 j=", j
          call sendrecv_detach(A(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               B(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               comm(j), omp_fulfill_event, ievent, ierr)
		!$omp end task
		!$omp task detach(ievent) depend(inout:A(j))
		  print *, "Inner Task2 j=", j
		  A(j) = A(j) + B(j)
          call sendrecv_detach(A(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               B(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               comm(j), omp_fulfill_event, ievent, ierr)
		!$omp end task
		!$omp task detach(ievent) depend(inout:A(j))
		  print *, "Inner Task3 j=", j
		  A(j) = A(j) + B(j)
          call sendrecv_detach(A(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               B(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               comm(j), omp_fulfill_event, ievent, ierr)
		!$omp end task
		!$omp task firstprivate(oevent) depend(inout:A(j))
		  print *, "Inner Task4 j=", j
		  A(j) = A(j) + B(j)
          call sendrecv_detach(A(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               B(j), 1, MPI_INTEGER, isize-irank-1, 1, &
                               comm(j), omp_fulfill_event, oevent, ierr)
		!$omp end task

    !$omp end task
    ENDDO
    !$omp taskwait
    !$omp end single
    !$omp end parallel
    
    print *, A(0), A(1), A(2), A(3), A(4), B    

    print *, "Goodbye from pe", irank, base

    CALL MPI_FINALIZE(ierr)

end program

subroutine sendrecv_detach(sendbuf, sendcount, sendtype, dest, sendtag, &
                           recvbuf, recvcount, recvtype, source, recvtag, &
                           comm, callback, cbdata, ierr)
    use omp_lib_kinds
    use mpi_detach
implicit none
INTEGER :: sendbuf(*), recvbuf(*)
INTEGER :: sendcount, sendtype, dest, sendtag
INTEGER :: recvcount, recvtype, source, recvtag, comm
INTEGER :: ierr
external :: callback
INTEGER (kind=omp_event_handle_kind), intent(in) :: cbdata
INTEGER :: reqs(0:1)
call MPI_Irecv(recvbuf, recvcount, recvtype, source, recvtag, comm, reqs(1), ierr);
call MPI_Isend(sendbuf, sendcount, sendtype, dest, sendtag, comm, reqs(0), ierr);
call MPIX_Detach_all_task(2, reqs, cbdata, ierr);
end subroutine sendrecv_detach

