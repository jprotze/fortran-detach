#include <unistd.h>
#include <omp.h>
#include <stdio.h>

void detach_task(void (*cb)(void*), void* data){
#pragma omp task
{
printf("Begin detach_task_  %i\n", omp_get_thread_num());
sleep(2);
printf("detach_task_ cb  %i\n", omp_get_thread_num());
cb(data);
printf("End detach_task_  %i\n", omp_get_thread_num());
}  
}

void detach_task_(void (*cb)(void*), void** data){
 detach_task(cb, data);
}