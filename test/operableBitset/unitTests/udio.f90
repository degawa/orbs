module test_operableBitset_unitTests_udio
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_type_operableBitset, only:operable_bitset
    use :: orbs_proc_error
    implicit none
    private
    public :: udo_write_bitset_in_binary_representation_to_a_unit
#if defined(NAGFOR)
    public :: udi_read_bitset_in_binary_representation_from_a_unit
#endif

contains
    subroutine udo_write_bitset_in_binary_representation_to_a_unit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        type(operable_bitset) :: bitset
        integer(int32) :: write_unit
        character(256) :: str_bitset
        logical :: stat
        character(:), allocatable :: msg

        !------------------------------------------------------------------!
        ! unformatted io
        bitset = operable_bitset("1111")
        open (newunit=write_unit, status="scratch")
        write (write_unit, *) bitset

        rewind (write_unit)
        read (write_unit, '(A)') str_bitset
        close (write_unit)

        call expect_equal(trim(adjustl(str_bitset)), "1111", &
                          "unformatted UDO `write (unit, *) bitset` with value of '1111' "// &
                          "should write '1111' to the unit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with incomplete format descriptor
        bitset = operable_bitset("11110000")
        open (newunit=write_unit, status="scratch")
        write (write_unit, '(DT)') bitset

        rewind (write_unit)
        read (write_unit, '(A)') str_bitset
        close (write_unit)

        call expect_equal(trim(adjustl(str_bitset)), "11110000", &
                          "formatted UDO `write (unit, '(DT)') bitset` with value of '11110000' "// &
                          "should write '11110000' to the unit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor
        bitset = operable_bitset("000011110000")
        open (newunit=write_unit, status="scratch")
        write (write_unit, '(DT"bitset")') bitset

        rewind (write_unit)
        read (write_unit, '(A)') str_bitset
        close (write_unit)

        call expect_equal(trim(adjustl(str_bitset)), "000011110000", &
                          "formatted UDO `write (unit, '(DT""bitset"")') bitset` with value of '000011110000' "// &
                          "should write '000011110000' to the unit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor and vlist
        bitset = operable_bitset("1111000011110000")
        open (newunit=write_unit, status="scratch")
        write (write_unit, '(DT"bitset"(16))') bitset

        rewind (write_unit)
        read (write_unit, '(A)') str_bitset
        close (write_unit)

        call expect_equal(trim(str_bitset), "1111000011110000", &
                          "formatted UDO `write (unit, '(DT""bitset""(16))') bitset` with value of '1111000011110000' "// &
                          "should write '1111000011110000' to the unit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor and longer vlist
        bitset = operable_bitset("1111000011110000")
        open (newunit=write_unit, status="scratch")
        write (write_unit, '(DT"bitset"(20))') bitset

        rewind (write_unit)
        read (write_unit, '(A)') str_bitset
        close (write_unit)

        call expect_equal(trim(str_bitset), "    1111000011110000", &
                          "formatted UDO `write (unit, '(DT""bitset""(20))') bitset` with value of '1111000011110000' "// &
                          "should write '    1111000011110000' to the unit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor and shorter vlist
        bitset = operable_bitset("1111000011110000")
        open (newunit=write_unit, status="scratch")
        write (write_unit, '(DT"bitset"(6))') bitset

        rewind (write_unit)
        read (write_unit, '(A)') str_bitset
        close (write_unit)

        call expect_equal(trim(str_bitset), "110000", &
                          "formatted UDO `write (unit, '(DT""bitset""(6))') bitset` with value of '1111000011110000' "// &
                          "should write '110000' to the unit", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine udo_write_bitset_in_binary_representation_to_a_unit

#if defined(NAGFOR)
    subroutine udi_read_bitset_in_binary_representation_from_a_unit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(operable_bitset) :: bitset
        integer(int32) :: read_unit
        logical :: stat
        character(:), allocatable :: msg

        !------------------------------------------------------------------!
        ! unformatted io
        open (newunit=read_unit, status="scratch")
        write (read_unit, '(A)') "10101010"
        rewind (read_unit)
        read (read_unit, *) bitset
        close (read_unit)

        call expect_equal(bitset%to_string(), "10101010", &
                          "unformatted UDI `read (unit, *) bitset` from the unit containing '10101010' "// &
                          "should read '10101010' into bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with incomplete format descriptor
        open (newunit=read_unit, status="scratch")
        write (read_unit, '(A)') "1101001110011111"
        rewind (read_unit)
        read (read_unit, '(DT)') bitset
        close (read_unit)

        call expect_equal(bitset%to_string(), "1101001110011111", &
                          "formatted UDI `read (unit, '(DT)') bitset` from the unit containing "// &
                          "'1101001110011111' should read '1101001110011111' into bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor
        open (newunit=read_unit, status="scratch")
        write (read_unit, '(A)') "00001101001110011111"
        rewind (read_unit)
        read (read_unit, '(DT"bitset")') bitset
        close (read_unit)

        call expect_equal(bitset%to_string(), "00001101001110011111", &
                          "formatted UDI `read (unit, '(DT""bitset"")') bitset` from the unit containing "// &
                          "'00001101001110011111' should read '1101001110011111' into bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor & vlist
        open (newunit=read_unit, status="scratch")
        write (read_unit, '(A)') "11110000"
        rewind (read_unit)
        read (read_unit, '(DT"bitset"(8))') bitset
        close (read_unit)

        call expect_equal(bitset%to_string(), "11110000", &
                          "formatted UDI `read (unit, '(DT""bitset""(8))') bitset` from the unit containing "// &
                          "'11110000' should read '11110000' into bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor & vlist
        open (newunit=read_unit, status="scratch")
        write (read_unit, '(A)') "11110000"
        rewind (read_unit)
        read (read_unit, '(DT"bitset"(10))') bitset
        close (read_unit)

        call expect_equal(bitset%to_string(), "0011110000", &
                          "formatted UDI `read (unit, '(DT""bitset""(10))') bitset` from the unit containing "// &
                          "'0011110000' should read '0011110000' into bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        !------------------------------------------------------------------!
        ! formatted io with format descriptor & vlist
        open (newunit=read_unit, status="scratch")
        write (read_unit, '(A)') "11110000"
        rewind (read_unit)
        read (read_unit, '(DT"bitset"(6))') bitset
        close (read_unit)

        call expect_equal(bitset%to_string(), "110000", &
                          "formatted UDI `read (unit, '(DT""bitset""(6))') bitset` from the unit containing "// &
                          "'110000' should read '110000' into bitset", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine udi_read_bitset_in_binary_representation_from_a_unit
#endif
end module test_operableBitset_unitTests_udio
