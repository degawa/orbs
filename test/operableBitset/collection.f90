module test_operableBitset_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_operableBitset_unitTests_initialize
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
                     ]
    end subroutine collect_operableBitset
end module test_operableBitset_collection
