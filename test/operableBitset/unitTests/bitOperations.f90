module test_operableBitset_unitTests_bitOperations
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset, only:operable_bitset
    implicit none
    private
    public :: set_pos_sets_one_bit
    public :: set_range_sets_bits_at_positions_from_start_to_end
    public :: clear_pos_clears_one_bit
    public :: clear_range_clears_bits_at_positions_from_start_to_end
    public :: flip_pos_flips_one_bit
    public :: flip_range_flips_bits_at_positions_from_start_to_end

contains
    subroutine set_pos_sets_one_bit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("000000000000000000000000")

        call bitset%set(0)
        call expect_true(bitset%test(0), &
                         "set(0) for '000000000000000000000000' should set 1 at 0-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%set(23)
        call expect_true(bitset%test(23), &
                         "set(23) for '000000000000000000000000' should set 1 at 23-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%set(24)
        call expect_true(bitset%bit_count() == 2, &
                         "set(24) for '000000000000000000000000' should be ignored", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%set(-1)
        call expect_true(bitset%bit_count() == 2, &
                         "set(-1) for '000000000000000000000000' should be ignored", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine set_pos_sets_one_bit

    subroutine set_range_sets_bits_at_positions_from_start_to_end(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("000000000000000000000000")

        call bitset%set(0, 2)
        call expect_true(all([bitset%test(0), bitset%test(1), bitset%test(2)]), &
                         "set(0, 2) for '000000000000000000000000' should set 1 from 0- to 2-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%set(11, 14)
        call expect_true(all([bitset%test(11), bitset%test(12), bitset%test(13), bitset%test(14)]), &
                         "set(11,14) for '000000000000000000000111' should set 1 from 11- to 14-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%set(23, 24)
        call expect_true(bitset%test(23), &
                         "set(MSB, MSB+1) for '000000000111100000000111' should set 1 at MSB-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine set_range_sets_bits_at_positions_from_start_to_end

    subroutine clear_pos_clears_one_bit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("111111111111111111111111111111111111111111111111")

        call bitset%clear(0)
        call expect_false(bitset%test(0), &
                          "clear(0) for '111111111111111111111111111111111111111111111111' "// &
                          "should set the bit to 0 at 0-bit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%clear(47)
        call expect_false(bitset%test(47), &
                          "clear(47) for '111111111111111111111111111111111111111111111110' "// &
                          "should set the bit to 0 at 47-bit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%clear(64)
        call expect_true(bitset%bit_count() == (48 - 2), &
                         "clear(64) for '011111111111111111111111111111111111111111111110' "// &
                         "should be ignored", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%clear(-1)
        call expect_true(bitset%bit_count() == (48 - 2), &
                         "clear(-1) for '011111111111111111111111111111111111111111111110' "// &
                         "should be ignored", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine clear_pos_clears_one_bit

    subroutine clear_range_clears_bits_at_positions_from_start_to_end(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("1111111111111111")

        call bitset%clear(0, 2)
        call expect_false(any([bitset%test(0), bitset%test(1), bitset%test(2)]), &
                          "clear(0, 2) for '1111111111111111' should set the bits to 0 from 0- to 2-bit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%clear(11, 14)
        call expect_false(any([bitset%test(11), bitset%test(12), bitset%test(13), bitset%test(14)]), &
                          "clear(11,14) for '1111111111111000' should set 1 from 11- to 14-bit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%clear(15, 23)
        call expect_false(bitset%test(15), &
                          "clear(MSB, MSB+8) for '1000011111111000'  should set the bit to 0 at MSB", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine clear_range_clears_bits_at_positions_from_start_to_end

    subroutine flip_pos_flips_one_bit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         5432109876543210
        bitset = operable_bitset("1011010010101110")

        call bitset%flip(0)
        call expect_true(bitset%test(0), &
                         "flip(0) for '1011010010101110' should set the bit to 1 at 0-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%flip(15)
        call expect_false(bitset%test(15), &
                          "flip(15) for '1011010010101111' should set the bit to 0 at 15-bit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%flip(24)
        call expect_true(bitset%bit_count() == 9, &
                         "flip(24) for '0011010010101111' should be ignored", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%clear(-1)
        call expect_true(bitset%bit_count() == 9, &
                         "flip(-1) for '0011010010101111' should be ignored", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine flip_pos_flips_one_bit

    subroutine flip_range_flips_bits_at_positions_from_start_to_end(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         5432109876543210
        bitset = operable_bitset("1011010010101110")

        call bitset%flip(0, 2)
        call expect_true(all([bitset%test(0)]) .and. .not. any([bitset%test(1), bitset%test(2)]), &
                         "clear(0, 2) for '1011010010101110' should flop the bits to 0 from 0- to 2-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%flip(11, 14)
        call expect_true(all([bitset%test(11), bitset%test(14)]) .and. .not. any([bitset%test(12), bitset%test(13)]), &
                         "clear(11, 14) for '1011010010101001' should flop the bits to 0 from 11- to 14-bit", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%flip(15, 23)
        call expect_false(bitset%test(15), &
                          "clear(MSB, MSB+8)for '1100110010101001' should flip the bit at MSB", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine flip_range_flips_bits_at_positions_from_start_to_end
end module test_operableBitset_unitTests_bitOperations
