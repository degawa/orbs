module test_proc_unitTests_sort
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_proc_error
    use :: orbs_type_operableBitset
    use :: strings_enclose
    implicit none
    private
    public :: sort_perform_sorting_input_array

contains
    subroutine sort_perform_sorting_input_array(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset), allocatable :: bitsets(:)
        logical :: stat
        character(:), allocatable :: msg

        bitsets = [operable_bitset("101"), operable_bitset("010"), operable_bitset("001")]
        call sort(bitsets)

        call expect_equal(bitsets(1)%to_string(), "001", &
                          "first element in the array ['101', '010', '001'] sorted in increasing order is '001'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(2)%to_string(), "010", &
                          "second element in the array ['101', '010', '001'] sorted in increasing order is '001'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(3)%to_string(), "101", &
                          "last element in the array ['101', '010', '001'] sorted in increasing order is '001'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call sort(bitsets, reverse=.true.)

        call expect_equal(bitsets(1)%to_string(), "101", &
                          "first element in the array ['001', '010', '101'] sorted in decreasing order is '101'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(2)%to_string(), "010", &
                          "second element in the array ['001', '010', '101'] sorted in increasing order is '010'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(3)%to_string(), "001", &
                          "last element in the array ['001', '010', '101'] sorted in increasing order is '001'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        deallocate (bitsets)

        bitsets = [operable_bitset(repeat("1", 128)), &
                   operable_bitset(repeat("1", 64)//repeat("0", 64)), &
                   operable_bitset(repeat("0", 64)//repeat("1", 64)), &
                   operable_bitset(repeat("0", 128))]
        call sort(bitsets)

        call expect_equal(bitsets(1)%to_string(), repeat("0", 128), &
                          "first element in the array sorted in increasing order is "// &
                          enclose(repeat("0", 128), "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(2)%to_string(), repeat("0", 64)//repeat("1", 64), &
                          "second element in the array sorted in increasing order is "// &
                          enclose(repeat("0", 64)//repeat("1", 64), "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(3)%to_string(), repeat("1", 64)//repeat("0", 64), &
                          "third element in the array sorted in increasing order is "// &
                          enclose(repeat("1", 64)//repeat("0", 64), "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitsets(4)%to_string(), repeat("1", 128), &
                          "last element in the array sorted in increasing order is "// &
                          enclose(repeat("1", 128), "'"), &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine sort_perform_sorting_input_array
end module test_proc_unitTests_sort
