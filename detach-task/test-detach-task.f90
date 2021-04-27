module detach
        interface
          subroutine detach_task (cb, event)
            use omp_lib_kinds
            external :: cb
            integer (kind=omp_event_handle_kind), &
              value, intent(in) :: event
          end subroutine detach_task
        end interface
end module
program test

    use omp_lib
    use detach

    implicit none

    integer(omp_event_handle_kind) :: event

!    external :: detach_task

    print *, "Hello "

    !$omp parallel num_threads(4)
    !$omp master
    !$omp task detach(event) depend(out:event)
    print *, "Task on thread", omp_get_thread_num()
    call detach_task(omp_fulfill_event, event)
    print *, "After detach_task", omp_get_thread_num()
    !$omp end task
    !$omp task depend(in:event)
    print *, "Dependent task", omp_get_thread_num()
    !$omp end task
    CALL SLEEP(1)
    !$omp taskwait
    !$omp end master
    !$omp end parallel
    
    

    print *,"Goodbye"

end program