module test_operableBitset_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_operableBitset_unitTests_initialize
    use :: test_operableBitset_unitTests_properties
    use :: test_operableBitset_unitTests_bitOperations
    use :: test_operableBitset_unitTests_getValue
    use :: test_operableBitset_unitTests_udio
    implicit none
    private
    public :: collect_operableBitset

contains
    subroutine collect_operableBitset(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("constructor", &
                                  constructor_returns_operable_bitset_type) &
                     , new_unittest("bits", &
                                    bits_returns_number_of_bits_of_bitset) &
                     , new_unittest("test - true", &
                                    test_returns_true_when_test_bit_is_1) &
                     , new_unittest("test - false", &
                                    test_returns_false_when_test_bit_is_0) &
                     , new_unittest("all - true", &
                                    all_returns_true_when_all_bits_are_1) &
                     , new_unittest("all - false", &
                                    all_returns_false_when_at_least_1bit_is_0) &
                     , new_unittest("any - true", &
                                    any_returns_true_when_any_bit_is_1) &
                     , new_unittest("any - false", &
                                    any_returns_false_when_all_bits_are_0) &
                     , new_unittest("none - true", &
                                    none_returns_true_when_all_bits_are_0) &
                     , new_unittest("none - false", &
                                    none_returns_false_when_at_least_1bit_is_1) &
                     , new_unittest("bit_count", &
                                    bit_count_returns_num_of_bits_that_are_1) &
                     , new_unittest("set position", &
                                    set_pos_sets_one_bit) &
                     , new_unittest("set range", &
                                    set_range_sets_bits_at_positions_from_start_to_end) &
                     , new_unittest("clear position", &
                                    clear_pos_clears_one_bit) &
                     , new_unittest("clear range", &
                                    clear_range_clears_bits_at_positions_from_start_to_end) &
                     , new_unittest("flip position", &
                                    flip_pos_flips_one_bit) &
                     , new_unittest("flip range", &
                                    flip_range_flips_bits_at_positions_from_start_to_end) &
                     , new_unittest("value 1", &
                                    value_returns_1_when_the_bit_is_1) &
                     , new_unittest("value 0", &
                                    value_returns_0_when_the_bit_is_0) &
                     , new_unittest("to_string", &
                                    to_string_returns_bitset_in_string) &
                     , new_unittest("extract", &
                                    extract_returns_specified_range_of_bitset) &
                     , new_unittest("user-defined derived type output", &
                                    udo_write_bitset_in_binary_representation_to_a_unit) &
#if defined(NAGFOR)
                     , new_unittest("user-defined derived type input", &
                                    udi_read_bitset_in_binary_representation_from_a_unit) &
#endif
                     ]
    end subroutine collect_operableBitset
end module test_operableBitset_collection
