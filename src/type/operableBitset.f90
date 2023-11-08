module orbs_type_operableBitset
    use, intrinsic :: iso_fortran_env
    use :: stdlib_bitsets, only:bitset_large, &
        and, &
        and_not, &
        or, &
        xor, &
        bitset_extract => extract, &
        operator(==), &
        operator(/=), &
        operator(>), &
        operator(>=), &
        operator(<), &
        operator(<=)
    use :: orbs_proc_error, only: &
        get_error_message, &
        success, &
        alloc_fault, &
        array_size_invalid_error, &
        char_string_invalid_error, &
        char_string_too_large_error, &
        char_string_too_small_error, &
        eof_failure, &
        index_invalid_error, &
        integer_overflow_error, &
        read_failure, &
        write_failure, &
        invalid_format_descriptor, &
        format_descriptor_mismatch
    implicit none
    private
    public :: success, &
              alloc_fault, &
              array_size_invalid_error, &
              char_string_invalid_error, &
              char_string_too_large_error, &
              char_string_too_small_error, &
              eof_failure, &
              index_invalid_error, &
              integer_overflow_error, &
              read_failure, &
              write_failure

    !>User-defined derived type to represent arithmetic operable bitset.
    type, public :: operable_bitset
        type(bitset_large), private :: bitset
            !! bitset
    contains
        ! initialization
        procedure, public, pass :: init_bits
        !* initialize the bitset by a number of bits
        procedure, public, pass :: init_string
        !* initialize the bitset by a string
        generic :: init => init_bits, init_string

        ! getting properties
        procedure, public, pass :: bits => get_number_of_bits
        !* returns number of bits in the bitset
        procedure, public, pass :: test
        !* determines whether a bit is set
        procedure, public, pass :: all => are_all_bits_1
        !* determines whether all bits are set
        procedure, public, pass :: any => are_any_bits_1
        !* determines whether any bits are set
        procedure, public, pass :: none => are_no_bits_1
        !* determines whether no bits are set
        procedure, public, pass :: bit_count
        !* returns the number of bits that are set

        ! bit operations
        procedure, public, pass :: set_pos
        !* sets one bit to 1
        procedure, public, pass :: set_range
        !* sets a sequence of bits to 1
        procedure, public, pass :: clear_pos
        !* clears one bit
        procedure, public, pass :: clear_range
        !* clears a sequence of bits
        procedure, public, pass :: flip_pos
        !* flips the value of a bit
        procedure, public, pass :: flip_range
        !* flips the values ofa sequence of bits
        generic :: set => set_pos, set_range
        generic :: clear => clear_pos, clear_range
        generic :: flip => flip_pos, flip_range

        ! getting values of bitset
        procedure, public, pass :: value => get_value_of_bit
        !* determines the value of a bit in the bitset
        procedure, public, pass :: to_string => bitset_to_string
        !* represents the bitset as a binary literal
        procedure, public, pass :: extract
        !* creates a new bitset from a range in the bitset
    end type operable_bitset

    !>Constructor for operable_bitset
    interface operable_bitset
        procedure :: construct_operable_bitset_bits
        procedure :: construct_operable_bitset_string
    end interface

contains
    !>Returns `operable_bitset` instance initialized by the numbere of bits.
    function construct_operable_bitset_bits(num_bits, status) result(new_bitset)
        integer(int32), intent(in) :: num_bits
            !! number of bits in the bitset to be initialized.
            !! `num_bits` must be greater than 0.
        integer(int32), intent(out), optional :: status
            !! initialization status
        type(operable_bitset) :: new_bitset
            !! new bitset instances

        call new_bitset%init(num_bits, status)
    end function construct_operable_bitset_bits

    !>Returns `operable_bitset` instance initialized by the string.
    function construct_operable_bitset_string(string, status) result(new_bitset)
        character(*), intent(in) :: string
            !! string to initialize the bitset
        integer(int32), intent(out), optional :: status
            !! initialization status
        type(operable_bitset) :: new_bitset
            !! new bitset instances

        call new_bitset%init(string, status)
    end function construct_operable_bitset_string

    !>Deep copys the bitset `from` to `to`.
    pure elemental subroutine clone(from, to)
        type(operable_bitset), intent(in) :: from
        type(operable_bitset), intent(out) :: to

        to = from
    end subroutine clone

    !>Initializes the bitset by the number of bits.
    subroutine init_bits(this, num_bits, status)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dumy argument
        integer(int32), intent(in) :: num_bits
            !! a number of bits for initializing the bitset
        integer(int32), intent(out), optional :: status
            !! initialization status

        call this%bitset%init_zero(num_bits, status)
    end subroutine init_bits

    !>Initializes the bitset by the string.
    subroutine init_string(this, string, status)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dumy argument
        character(*), intent(in) :: string
            !! string to initialize the bitset
        integer(int32), intent(out), optional :: status
            !! initialization status

        call this%bitset%from_string(string, status)
    end subroutine init_string

    !>Returns the number of bits in the bitset.
    pure integer(int32) function get_number_of_bits(this)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument

        get_number_of_bits = this%bitset%bits()
    end function get_number_of_bits

    !>Returns `.true.` if all bits are set to 1,
    !>and returns `.false.` otherwise.
    pure logical function are_all_bits_1(this)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument

        are_all_bits_1 = this%bitset%all()
    end function are_all_bits_1

    !>Returns `.true.` if at least a bit is set to 1,
    !>and returns `.false.` otherwise.
    pure logical function are_any_bits_1(this)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument

        are_any_bits_1 = this%bitset%any()
    end function are_any_bits_1

    !>Returns `.true.` if no bits are set to 1,
    !>and returns `.false.` otherwise.
    pure logical function are_no_bits_1(this)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument

        are_no_bits_1 = this%bitset%none()
    end function are_no_bits_1

    !>Retruns the number of bits that are set to 1.
    pure integer(int32) function bit_count(this)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument

        bit_count = this%bitset%bit_count()
    end function bit_count

    !>Sets a bit at `pos` to 1.
    subroutine set_pos(this, pos)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: pos
            !! position of the bit to be set

        call this%bitset%set(pos)
    end subroutine set_pos

    !>Sets a sequence of bits from `start_pos` to `end_pos` to 1.
    subroutine set_range(this, start_pos, end_pos)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: start_pos
            !! starting position of the range for setting bits
        integer(int32), intent(in) :: end_pos
            !! end position of the range for setting bits

        call this%bitset%set(start_pos, end_pos)
    end subroutine set_range

    !>Clears a bit at `pos` to 0.
    subroutine clear_pos(this, pos)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: pos
            !! position of the bit to be cleared

        call this%bitset%clear(pos)
    end subroutine clear_pos

    !>Clears a sequence of bits from `start_pos` to `end_pos` to 0.
    subroutine clear_range(this, start_pos, end_pos)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: start_pos
            !! starting position of the range for clearing bits
        integer(int32), intent(in) :: end_pos
            !! end position of the range for clearing bits

        call this%bitset%clear(start_pos, end_pos)
    end subroutine clear_range

    !>Flips the value of a bit at `pos`.
    subroutine flip_pos(this, pos)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: pos
            !! position of the bit to be fliped

        call this%bitset%flip(pos)
    end subroutine flip_pos

    !>Flips the values of a sequence of bits from `start_pos` to `end_pos`.
    subroutine flip_range(this, start_pos, end_pos)
        class(operable_bitset), intent(inout) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: start_pos
            !! starting position of the range for flipping bits
        integer(int32), intent(in) :: end_pos
            !! end position of the range for flipping bits

        call this%bitset%flip(start_pos, end_pos)
    end subroutine flip_range

    !>Returns `.true.` if the bit at `pos` is set to 1,
    !>and returns `.false.` otherwise.
    pure logical function test(this, pos)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: pos
            !! position of the bit to be tested

        test = this%bitset%test(pos)
    end function test

    !>Returns `1` if the bit at `pos` is set to 1,
    !>and returns `0` otherwise.
    pure function get_value_of_bit(this, pos) result(bit_value)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument
        integer(int32), intent(in) :: pos
            !! position of the bit to get to value
        integer(int32) :: bit_value
            !! the value of the bit

        bit_value = this%bitset%value(pos)
    end function get_value_of_bit

    !>Returns the value of bitset as a binary literal in string.
    function bitset_to_string(this) result(string)
        class(operable_bitset), intent(in) :: this
            !! passed-object dummy argument
        character(:), allocatable :: string
            !! string containing the vlaue of bitset as a binary literal

        call this%bitset%to_string(string)
    end function bitset_to_string

    !>Returns new bitset instance from a range, `start_pos` to `end_pos`,
    !>in the bitset.
    function extract(this, start_pos, end_pos) result(new_bitset)
        class(operable_bitset), intent(in) :: this
        integer(int32), intent(in) :: start_pos
            !! starting position of the range
        integer(int32), intent(in) :: end_pos
            !! end position of the range
        type(operable_bitset) :: new_bitset
            !! new bitset instance constructed by the extracted bitset.

        call bitset_extract(new_bitset%bitset, this%bitset, start_pos, end_pos)
    end function extract
end module orbs_type_operableBitset