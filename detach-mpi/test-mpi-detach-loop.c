#include "mpi-detach.h"
#include <mpi.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void sendrecv_detach(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                     int dest, int sendtag, void *recvbuf, int recvcount,
                     MPI_Datatype recvtype, int source, int recvtag,
                     MPI_Comm comm, MPIX_Detach_callback *callback,
                     void *cbdata) {
  MPI_Request reqs[2];
  MPI_Irecv(recvbuf, recvcount, recvtype, source, recvtag, comm, reqs + 1);
  MPI_Isend(sendbuf, sendcount, sendtype, dest, sendtag, comm, reqs);
  MPIX_Detach_all(2, reqs, callback, cbdata);
//  MPIX_Detach(reqs+1, callback, cbdata);
/*  MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, comm, MPI_STATUS_IGNORE);
  callback(cbdata);*/
}

int main(int argc, char **argv) {
  int provided;
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  if (provided != MPI_THREAD_MULTIPLE) {
    printf("This code needs MPI_THREAD_MULTIPLE(%i), threadlevel %i was "
           "provided\n",
           MPI_THREAD_MULTIPLE, provided);
    MPI_Finalize();
    return -1;
  }
  int rank, size;
  MPI_Comm comm[5];
  for (int i = 0; i < 5; i++) 
    MPI_Comm_dup(MPI_COMM_WORLD, comm+i);
  
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  int A[] = {1, 2, 3, 4, 5};
  int B[5] = {-1,-1,-1,-1,-1};
  omp_event_handle_t oevent;

#pragma omp parallel num_threads(8)
#pragma omp single
  {
    for (int i = 0; i < 1; i++) {
      int j = i % 5;
#pragma omp task depend(inout : B[j]) detach(oevent)
      {
        omp_event_handle_t ievent;
#pragma omp task depend(inout : B[j]) detach(ievent)
        sendrecv_detach(A + j, 1, MPI_INT, size - rank - 1, 1, B + j, 1,
                        MPI_INT, size - rank - 1, 1, comm[j],
                        (MPIX_Detach_callback *)omp_fulfill_event,
                        (void *)ievent);
#pragma omp task depend(inout : B[j]) detach(ievent)
        {
          printf("task 2 %i: A=%i, B=%i\n", j, A[j], B[j]);
          A[j] += B[j];
          sendrecv_detach(A + j, 1, MPI_INT, size - rank - 1, 2, B + j, 1,
                          MPI_INT, size - rank - 1, 2, comm[j],
                          (MPIX_Detach_callback *)omp_fulfill_event,
                          (void *)ievent);
        }
#pragma omp task depend(inout : B[j]) detach(ievent)
        {
          printf("task 3 %i: A=%i, B=%i\n", j, A[j], B[j]);
          A[j] += B[j];
          sendrecv_detach(A + j, 1, MPI_INT, size - rank - 1, 4, B + j, 1,
                          MPI_INT, size - rank - 1, 4, comm[j],
                          (MPIX_Detach_callback *)omp_fulfill_event,
                          (void *)ievent);
        }
#pragma omp task depend(inout : B[j]) firstprivate(oevent)
        {
          printf("task 4 %i: A=%i, B=%i\n", j, A[j], B[j]);
          A[j] += B[j];
          sendrecv_detach(A + j, 1, MPI_INT, size - rank - 1, 5, B + j, 1,
                          MPI_INT, size - rank - 1, 5, comm[j],
                          (MPIX_Detach_callback *)omp_fulfill_event,
                          (void *)oevent);
        }
      }
    }
#pragma omp taskwait
  }
  printf("%i, %i, %i\n", rank, B[0], B[3]);
  MPI_Finalize();
}
