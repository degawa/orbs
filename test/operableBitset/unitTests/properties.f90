module test_operableBitset_unitTests_properties
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset, only:operable_bitset
    implicit none
    private
    public :: bits_returns_number_of_bits_of_bitset
    public :: test_returns_true_when_test_bit_is_1
    public :: test_returns_false_when_test_bit_is_0
    public :: all_returns_true_when_all_bits_are_1
    public :: all_returns_false_when_at_least_1bit_is_0
    public :: any_returns_true_when_any_bit_is_1
    public :: any_returns_false_when_all_bits_are_0
    public :: none_returns_true_when_all_bits_are_0
    public :: none_returns_false_when_at_least_1bit_is_1
    public :: bit_count_returns_num_of_bits_that_are_1

contains
    subroutine bits_returns_number_of_bits_of_bitset(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset(27)
        call expect_equal(bitset%bits(), 27, &
                          "bits() should return number of bits in bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine bits_returns_number_of_bits_of_bitset

    subroutine test_returns_true_when_test_bit_is_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         109876543210
        bitset = operable_bitset("101100111000")

        call expect_true(bitset%test(3), &
                         "test(3) for '101100111000' should return `.true.`", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(bitset%test(4), &
                         "test(4) for '101100111000' should return `.true.`", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(bitset%test(5), &
                         "test(5) for '101100111000' should return `.true.`", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(bitset%test(8), &
                         "test(8) for '101100111000' should return `.true.`", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(bitset%test(9), &
                         "test(9) for '101100111000' should return `.true.`", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(bitset%test(11), &
                         "test(11) for '101100111000' should return `.true.`", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine test_returns_true_when_test_bit_is_1

    subroutine test_returns_false_when_test_bit_is_0(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         109876543210
        bitset = operable_bitset("101100111000")

        call expect_false(bitset%test(0), &
                          "test(0) for '101100111000' should return `.false.`", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_false(bitset%test(1), &
                          "test(1) for '101100111000' should return `.false.`", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_false(bitset%test(2), &
                          "test(2) for '101100111000' should return `.false.`", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_false(bitset%test(6), &
                          "test(6) for '101100111000' should return `.false.`", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_false(bitset%test(7), &
                          "test(7) for '101100111000' should return `.false.`", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_false(bitset%test(10), &
                          "test(10) for '101100111000' should return `.false.`", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine test_returns_false_when_test_bit_is_0

    subroutine all_returns_true_when_all_bits_are_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("1111111111111111111111111111111111111111111111111111111111111111")
        call expect_true(bitset%all(), &
                         "all() for '1111111111111111111111111111111111111111111111111111111111111111' "// &
                         "should return `.true.` if all bits are 1", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine all_returns_true_when_all_bits_are_1

    subroutine all_returns_false_when_at_least_1bit_is_0(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("11101110111101111011001111100001")
        call expect_false(bitset%all(), &
                          "all() for '11101110111101111011001111100001' "// &
                          "should return `.false.` if at least 1 bit is not 1", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine all_returns_false_when_at_least_1bit_is_0

    subroutine any_returns_true_when_any_bit_is_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("11101110111101111011001111100001")
        call expect_true(bitset%any(), &
                         "any() for '11101110111101111011001111100001'"// &
                         " should return `.true.` if any bit is 1", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine any_returns_true_when_any_bit_is_1

    subroutine any_returns_false_when_all_bits_are_0(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("0000000000000000000000000000000000000000000000000000000000000000")
        call expect_false(bitset%any(), &
                          "any() for '0000000000000000000000000000000000000000000000000000000000000000' "// &
                          "should return `.false.` if all bits are 0", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine any_returns_false_when_all_bits_are_0

    subroutine none_returns_true_when_all_bits_are_0(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("0000000000000000000000000000000000000000000000000000000000000000")
        call expect_true(bitset%none(), &
                         "none() for '0000000000000000000000000000000000000000000000000000000000000000' "// &
                         "should return `.true.` if all bits are 0", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine none_returns_true_when_all_bits_are_0

    subroutine none_returns_false_when_at_least_1bit_is_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        bitset = operable_bitset("0000000000000000000000000000000000000000000001000000000000000000")
        call expect_false(bitset%none(), &
                          "none() for '0000000000000000000000000000000000000000000001000000000000000000' "// &
                          "should return `.false.` if at least 1 bit is 1", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine none_returns_false_when_at_least_1bit_is_1

    subroutine bit_count_returns_num_of_bits_that_are_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        logical :: stat
        character(:), allocatable :: msg

        !                         1 234    56 7 8  90   123 4 567 8 90123 4  5   6  78 9 0 12 3 4 5
        bitset = operable_bitset("101110000110101001100011101011101011111010010001001101010110101010")
        call expect_equal(bitset%bit_count(), 35, &
                          "bit_cout() for '101110000110101001100011101011101011111010010001001101010110101010' "// &
                          "should return 35", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bitset = operable_bitset("000000000000000000000000000000000000000000000000")
        call expect_equal(bitset%bit_count(), 0, &
                          "bit_cout() for '000000000000000000000000000000000000000000000000' "// &
                          "should return 0", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        bitset = operable_bitset("1111111111111111111111111111111111111111111111111111111111111111")
        call expect_equal(bitset%bit_count(), 64, &
                          "bit_cout() '1111111111111111111111111111111111111111111111111111111111111111' "// &
                          "should return 64", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine bit_count_returns_num_of_bits_that_are_1
end module test_operableBitset_unitTests_properties
