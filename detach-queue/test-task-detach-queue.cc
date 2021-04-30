#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <ctime>
#include <iostream>
#include <list>
#include <mutex>
#include <thread>

using namespace std::literals::chrono_literals;

struct WorkItem {
  omp_event_handle_t event;
  std::chrono::time_point<std::chrono::steady_clock> duetime;
  const int *a;
  int *b;
  int id;
};
static std::mutex workQueueMtx;
static std::list<WorkItem> workQueue;
static std::atomic<bool> running{true};
static std::atomic<int> workId{0};

template <class R, class D>
void queue_detach(const int *a, int *b, omp_event_handle_t event,
                  std::chrono::duration<R, D> workLoad) {
  WorkItem work{event, std::chrono::steady_clock::now() + workLoad, a, b,
                ++workId};
  std::unique_lock<std::mutex> lck(workQueueMtx);
  workQueue.emplace_back(work);
}

void run(void) {
  while (running) {
    {
      std::unique_lock<std::mutex> lck(workQueueMtx);
      while (!workQueue.empty() &&
             workQueue.front().duetime < std::chrono::steady_clock::now()) {
        *workQueue.front().b = *workQueue.front().a;
        std::cout << "Done with work " << workQueue.front().id << std::endl;
        omp_fulfill_event(workQueue.front().event);
        workQueue.pop_front();
      }
    }
    std::this_thread::sleep_for(1ms);
  }
}

int main(int argc, char **argv) {
  int A[] = {1, 2, 3, 4, 5};
  int B[] = {-1, -1, -1, -1, -1};
  omp_event_handle_t oevent, ievent;

  std::thread runThread{run};

#pragma omp parallel num_threads(4)
#pragma omp single
  {
    for (int i = 0; i < 4; i++) {
      int j = i % 2;
#pragma omp task depend(inout : A[j], B[j]) detach(oevent)
      {
#pragma omp task depend(inout : A[j], B[j]) detach(ievent)
        queue_detach(A + j, B + j, ievent, 9ms);
#pragma omp task depend(inout : A[j], B[j]) detach(ievent)
        {
          A[j] += B[j];
          queue_detach(A + j, B + j, ievent, 5ms);
        }
#pragma omp task depend(inout : A[j], B[j]) detach(ievent)
        {
          A[j] += B[j];
          queue_detach(A + j, B + j, ievent, 11ms);
        }
#pragma omp task depend(inout : A[j], B[j]) firstprivate(oevent)
        {
          A[j] += B[j];
          queue_detach(A + j, B + j, oevent, 7ms);
        }
      }
    }
#pragma omp taskwait
  }
  printf("%i, %i, %i, %i, %i\n", B[0], B[1], B[2], B[3], B[4]);
  running = false;
  runThread.join();
}
