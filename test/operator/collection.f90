module test_operator_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_operator_unitTests_logical
    use :: test_operator_unitTests_comparison
    use :: test_operator_unitTests_arithmetic
    use :: test_operator_unitTests_bit
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
                     , new_unittest("bitset comparator == true", &
                                    eq_op_returns_true_if_both_sides_have_same_bitset) &
                     , new_unittest("bitset comparator == false", &
                                    eq_op_returns_false_if_both_sides_have_different_bitset) &
                     , new_unittest("bitset comparator /= true", &
                                    neq_op_returns_true_if_both_sides_have_different_bitset) &
                     , new_unittest("bitset comparator /= false", &
                                    neq_op_returns_false_if_both_sides_have_same_bitset) &
                     , new_unittest("bitset comparator > true", &
                                    gt_op_returns_true_if_lhs_is_greater_than_rhs) &
                     , new_unittest("bitset comparator > false", &
                                    gt_op_returns_false_if_lhs_is_less_than_or_equal_to_rhs) &
                     , new_unittest("bitset comparator >= true", &
                                    ge_op_returns_true_if_lhs_is_greater_than_or_equal_to_rhs) &
                     , new_unittest("bitset comparator >= false", &
                                    ge_op_returns_false_if_lhs_is_less_than_rhs) &
                     , new_unittest("bitset comparator < true", &
                                    lt_op_returns_true_if_lhs_is_less_than_rhs) &
                     , new_unittest("bitset comparator < false", &
                                    lt_op_returns_false_if_lhs_is_greater_than_or_equal_to_rhs) &
                     , new_unittest("bitset comparator <= true", &
                                    le_op_returns_true_if_lhs_is_less_than_or_equal_to_rhs) &
                     , new_unittest("bitset comparator <= false", &
                                    le_op_returns_false_if_lhs_is_greater_than_rhs) &
                     , new_unittest("bitset arithmetic operator +", &
                                    add_op_returns_bitset_having_value_of_result_of_addition) &
                     , new_unittest("bitset arithmetic operator -", &
                                    sub_op_returns_bitset_having_value_of_result_of_subtraction) &
                     , new_unittest("bitset shift operator", &
                                    shift_op_returns_bitset_having_value_of_result_of_shifting) &
                     ]
    end subroutine collect_operator
end module test_operator_collection
