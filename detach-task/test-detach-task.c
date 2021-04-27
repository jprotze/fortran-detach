#include <omp.h>
#include <stdio.h>
#include <unistd.h>

void detach_task(void (*cb)(void*), void* data);
int main(){
#pragma omp parallel num_threads(4)
    #pragma omp master
    {
    omp_event_handle_t event; 
    #pragma omp task detach(event) depend(out:event)
    {
    printf( "Task on thread %i\n", omp_get_thread_num());
    detach_task((void (*)(void*)) omp_fulfill_event, (void*)event);
    printf( "After detach_task %i\n", omp_get_thread_num());
    }
    #pragma omp task depend(in:event)
    {
    printf( "Dependent task %i\n", omp_get_thread_num());
    }
    sleep(1);
    #pragma omp taskwait
}
}