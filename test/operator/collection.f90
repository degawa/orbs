module test_operator_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_operator_unitTests_logical
    implicit none
    private
    public :: collect_operator

contains
    subroutine collect_operator(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("bitsset operation logical AND", &
                                  and_op_compute_bitwise_logical_and) &
                     , new_unittest("bitset operation logical OR", &
                                    or_op_compute_bitwise_logical_or) &
                     , new_unittest("bitset operation logical XOR", &
                                    xor_op_compute_bitwise_logical_exclusive_or) &
                     , new_unittest("bitset operation bitwise logical NOT", &
                                    not_op_compute_bitwise_logical_complement) &
                     , new_unittest("bitset operation bitwise logical AND of a and NOT b", &
                                    andnot_op_compute_logical_and_of_a_and_not_b) &
                     ]
    end subroutine collect_operator
end module test_operator_collection
