program ex_bitset
    use, intrinsic :: iso_fortran_env
    use :: orbs
    implicit none

    type(operable_bitset) :: bitset, one
    integer(int32) :: status

    bitset = operable_bitset("11111111")
    write (*, *) bitset ! 11111111
    write (*, '(DT"bitset"(10))') bitset !   11111111

    one = orbs_one(bitset)
    print *, bitset + one ! 00000000
    print '(DT"bitset"(8))', bitset.shift.2 ! 11111100

    call bitset%init("2", status)
    print *, orbs_get_error_message(status) !  A character string had an invalid character.

    bitset = "00001111"
    print *, bitset ! 00001111

    print *, operable_bitset("111")//operable_bitset("00000")
    ! 11100000
end program ex_bitset
