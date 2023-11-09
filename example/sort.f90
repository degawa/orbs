program ex_sort
    use :: orbs
    implicit none

    type(operable_bitset), allocatable :: bitset(:)

    allocate (bitset(3))
    bitset(1) = operable_bitset("100")
    bitset(2) = operable_bitset("001")
    bitset(3) = operable_bitset("010")

    call sort(bitset)
    print *, bitset ! 001 010 100
end program ex_sort
