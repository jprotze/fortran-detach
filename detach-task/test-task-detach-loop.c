#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void task_detach(const int *sendbuf, int *recvbuf, omp_event_handle_t event) {
  #pragma omp task firstprivate(event)
  {
    usleep(1000);
    *recvbuf = *sendbuf;
    omp_fulfill_event(event);
  } 
}

int main(int argc, char **argv) {
  int A[] = {1, 2, 3, 4, 5};
  int B[] = {-1,-1,-1,-1,-1};
  omp_event_handle_t oevent, ievent;

#pragma omp parallel num_threads(12)
#pragma omp single
  {
    for (int i = 0; i < 15; i++) {
      int j = i % 5;
#pragma omp task depend(inout : A[j], B[j]) detach(oevent) firstprivate(j, ievent)
      {
#pragma omp task depend(inout : A[j], B[j]) detach(ievent)
        task_detach(A + j, B + j,
                        ievent);
#pragma omp task depend(inout : A[j], B[j]) detach(ievent)
        {
          A[j] += B[j];
        task_detach(A + j, B + j,
                        ievent);
        }
#pragma omp task depend(inout : A[j], B[j]) detach(ievent)
        {
          A[j] += B[j];
        task_detach(A + j, B + j,
                        ievent);
        }
#pragma omp task depend(inout : A[j], B[j]) firstprivate(oevent)
        {
          A[j] += B[j];
        task_detach(A + j, B + j,
                          oevent);
        }
      }
    }
#pragma omp taskwait
  }
  printf("%i, %i, %i, %i, %i\n", B[0], B[1], B[2], B[3], B[4]);
}
