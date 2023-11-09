program ex_key
    use :: orbs
    implicit none

    type(operable_bitset) :: key, one, mask_x, mask_y

    key = operable_bitset("0011")
    one = orbs_one(mold=key)
    mask_x = operable_bitset("1010")
    mask_y = operable_bitset("0101")

    ! find off-branch neighbours
    print *, (((key .or. mask_y) + one) .and. mask_x) .or. (key .and. mask_y)
    ! 1001

end program ex_key
