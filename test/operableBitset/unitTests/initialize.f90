module test_operableBitset_unitTests_initialize
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset, only:operable_bitset, success, char_string_invalid_error
    implicit none
    private
    public :: constructor_returns_operable_bitset_type

contains
    subroutine constructor_returns_operable_bitset_type(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: mold
        logical :: stat
        character(:), allocatable :: msg

        ! construct operable_bitset type by number of bits
        call expect_true(same_type_as(operable_bitset(33), mold), &
                         "operable_bitset(num_bits) should return operable_bitset type", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! construct operable_bitset type by number of bits
        call expect_true(same_type_as(operable_bitset("00101011"), mold), &
                         "operable_bitset(string) should return operable_bitset type", &
                         stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine constructor_returns_operable_bitset_type

    subroutine init_initialize_component_bitset(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        integer(int32) :: status
        logical :: stat
        character(:), allocatable :: msg

        ! init(num_bits, status)
        call bitset%init(64, status)

        call expect_equal(status, success, &
                          "init(num_bits, status) should have status euqal to `success` in case of success", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%bits(), 64, &
                          "init(num_bits,stat) should allocate num_bits-bit bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! init(string, status)
        call bitset%init("0010100001110001010", status)

        call expect_equal(status, success, &
                          "init(string, status) should have status euqal to `success` in case of success", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(bitset%bits(), len("0010100001110001010"), &
                          "init(string,stat) should allocate len(string)-bit bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call bitset%init("2", status)

        call expect_equal(status, char_string_invalid_error, &
                          "init(string, status) should have status euqal to `char_string_invalid_error`"// &
                          " if string has invalid characters", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine init_initialize_component_bitset
end module test_operableBitset_unitTests_initialize
