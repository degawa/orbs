module test_proc_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_proc_unitTests_error
    implicit none
    private
    public :: collect_proc

contains
    subroutine collect_proc(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
        !! collection of tests

        test_suite = [ &
                     new_unittest("error", &
                                  get_error_message_returns_errmsg) &
                     ]
    end subroutine collect_proc
end module test_proc_collection
