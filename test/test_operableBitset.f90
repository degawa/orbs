program test_operableBitset
    use :: test_proc_collection
    use :: test_operableBitset_collection
    use :: test_operator_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("procs for operable bitset", collect_proc) &
                  , new_testsuite("operable_bitset type", collect_operableBitset) &
                  , new_testsuite("bitset operations", collect_operator) &
                  ]
    call run_test(test_suites)
end program test_operableBitset
