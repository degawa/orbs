module test_proc_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_proc_unitTests_error
    use :: test_proc_unitTests_literal
    use :: test_proc_unitTests_sort
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
                     , new_unittest("literal 1", &
                                    one_returns_1_in_binary_representation) &
                     , new_unittest("literal 0", &
                                    zero_returns_0_in_binary_representation) &
                     , new_unittest("sorting", &
                                    sort_perform_sorting_input_array) &
                     ]
    end subroutine collect_proc
end module test_proc_collection
