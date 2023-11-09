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
    public :: operator( .and. )
    public :: operator(.andnot.)
    public :: operator( .or. )
    public :: operator(.xor.)
    public :: operator(.not.)
    public :: operator(==)
    public :: operator(/=)
    public :: operator(>)
    public :: operator(>=)
    public :: operator(<)
    public :: operator(<=)
    public :: operator(+)
    public :: operator(-)
    public :: operator(.shift.)
    public :: sort
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

        ! UDIO
        procedure, public, pass :: output
        !* writes a binary representation of the bitset to a unit
        generic :: write (formatted) => output
    end type operable_bitset

    !>Constructor for operable_bitset
    interface operable_bitset
        procedure :: construct_operable_bitset_bits
        procedure :: construct_operable_bitset_string
    end interface

    !>Bitwise logical AND of the bits of two bitsets.
    interface operator( .and. )
        procedure :: and_bitset_bitset
    end interface

    !>Bitwise logical AND of one bitset with the negation of another.
    interface operator(.andnot.)
        procedure :: and_not_bitset_bitset
    end interface

    !>Bitwise logical OR of the bits of two bitsets.
    interface operator( .or. )
        procedure :: or_bitset_bitset
    end interface

    !>Bitwise logical exclusive OR of the bits of two bitsets.
    interface operator(.xor.)
        procedure :: xor_bitset_bitset
    end interface

    !>Bitwise logical complement of the bits.
    interface operator(.not.)
        procedure :: not_bitset
    end interface

    !>Comparison of two bitsets to determine
    !>whether lhs and rhs have the same value.
    interface operator(==)
        procedure :: is_equal_bitset_bitset
    end interface

    !>Comparison of two bitsets to determine
    !>whether lhs and rhs differ from each other.
    interface operator(/=)
        procedure :: is_not_equal_bitset_bitset
    end interface

    !>Comparison of two bitsets to determine
    !>whether lhs is greater than rhs.
    interface operator(>)
        procedure :: is_greater_than_bitset_bitset
    end interface

    !>Comparison of two bitsets to determine
    !>whether lhs is greater than or equal to rhs.
    interface operator(>=)
        procedure :: is_greater_than_or_equal_to_bitset_bitset
    end interface

    !>Comparison of two bitsets to determine
    !>whether lhs is less than rhs.
    interface operator(<)
        procedure :: is_less_than_bitset_bitset
    end interface

    !>Comparison of two bitsets to determine
    !>whether lhs is less than or equal to rhs.
    interface operator(<=)
        procedure :: is_less_than_or_equal_to_bitset_bitset
    end interface

    !>Addition of two bitsets.
    interface operator(+)
        procedure :: add_bitset_bitset
    end interface

    !>Subtraction of two bitsets.
    interface operator(-)
        procedure :: sub_bitset_bitset
    end interface

    !>Bit shift on bitset.
    interface operator(.shift.)
        procedure :: shift_bitset
    end interface

    !>Sorting a bitset array.
    interface sort
        procedure :: sort_bitset
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

    !>Writes a bitset representation of the bitset to a unit.
    subroutine output(bitset, unit, io_type, v_list, io_status, io_message)
        use :: stdlib_ascii, only:to_upper

        class(operable_bitset), intent(in) :: bitset
            !! passed-object dummy argument
        integer(int32), intent(in) :: unit
            !! unit number
        character(*), intent(in) :: io_type
            !! type name specified by the format descriptor for UDIO
        integer(int32), intent(in) :: v_list(:)
            !! variable list specified by the format descriptor for UDIO
        integer(int32), intent(out) :: io_status
            !! IO status returned from write statement
        character(*), intent(inout) :: io_message
            !! IO message returned from write statement

        error_case: block
            !- fmt /= '(DT"bitset")'
            if (all([io_type /= "LISTDIRECTED", &
                     io_type /= "DT", &
                     to_upper(io_type) /= "DTBITSET"])) then
                io_status = format_descriptor_mismatch
                io_message = get_error_message(io_status)
                return
            end if
        end block error_case

        !--- normal case
        normal_case: block
            character(:), allocatable :: str_bitset, fmt
            character(11) :: str_digits
            integer(int32) :: num_bits

            num_bits = bitset%bits()
            if (num_bits >= 1) then
                str_bitset = bitset%to_string()
            else
                str_bitset = ""
            end if

            !--- unformatted write
            if (any([io_type == "LISTDIRETED", & ! fmt = *
                     to_upper(io_type) == "DT", & ! fmt = '(DT)'
                     size(v_list) < 1])) then ! fmt = '(DT"bitset")'

                write (unit, '(A)', iostat=io_status, iomsg=io_message) str_bitset
                return
            end if

            !--- formatted write
            ! size(v_list) < 1 is captured at the if stmt above.
            write (str_digits, '(I0)') v_list(1)
            fmt = '(A'//trim(str_digits)//')'

            if (v_list(1) > num_bits) then
                ! padding the upper bits with " "
                ! write(*, DT"bitset"(10)) 11111111 writes   11111111
                !                                          ^--------^
                str_bitset = repeat(" ", v_list(1) - num_bits)//str_bitset
            else if (v_list(1) < num_bits) then
                ! truncate upper bits
                ! write(*, DT"bitset"(6)) 11000000 writes (11)000000
                !                                             ^----^
                str_bitset = str_bitset(num_bits - v_list(1) + 1:)
            end if

            write (unit, fmt, iostat=io_status, iomsg=io_message) str_bitset
        end block normal_case
    end subroutine output

    !>Returns new `operable_bitset` instance having the bits
    !>set to the bitwise logical AND of the bits in `lhs` and `rhs`.
    !>
    !>This procedure intented to be overloaded as the `.and.` operator.
    function and_bitset_bitset(lhs, rhs) result(new_bitset)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of the AND operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of the AND operator
        type(operable_bitset) :: new_bitset
            !! new bitset having bits set to the bitwise logical AND

        call clone(from=lhs, to=new_bitset)
        call and(new_bitset%bitset, rhs%bitset)
    end function and_bitset_bitset

    !>Returns new `operable_bitset` instance having the bits
    !>set to the bitwise logical AND of the bits in `lhs`
    !>with the bitwise negation of the corresponding bits in `rhs`.
    !>
    !>This procedure intented to be overloaded as the `.andnot.` operator.
    elemental function and_not_bitset_bitset(lhs, rhs) result(new_bitset)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of the ANDNOT operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the left side of the ANDNOT operator
        type(operable_bitset) :: new_bitset
            !! new bitset having bits set to the bitwise logical AND
            !! of `lhs` and the negation of `rhs`

        call clone(from=lhs, to=new_bitset)
        call and_not(new_bitset%bitset, rhs%bitset)
    end function and_not_bitset_bitset

    !>Returns new `operable_bitset` instance having the bits
    !>set to the bitwise logical OR of the bits in `lhs` and `rhs`.
    !>
    !>This procedure intented to be overloaded as the `.or.` operator.
    function or_bitset_bitset(lhs, rhs) result(new_bitset)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of the OR operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of the OR operator
        type(operable_bitset) :: new_bitset
            !! new bitset having bits set to the bitwise logical OR

        call clone(from=lhs, to=new_bitset)
        call or(new_bitset%bitset, rhs%bitset)
    end function or_bitset_bitset

    !>Returns new `operable_bitset` instance having the bits
    !>set to the bitwise logical exclusive OR of the bits in `lhs` and `rhs`.
    !>
    !>This procedure intented to be overloaded as the `.xor.` operator.
    elemental function xor_bitset_bitset(lhs, rhs) result(new_bitset)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of the XOR operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of the XOR operator
        type(operable_bitset) :: new_bitset
            !! new bitset having bits set to the bitwise logical XOR

        call clone(from=lhs, to=new_bitset)
        call xor(new_bitset%bitset, rhs%bitset)
    end function xor_bitset_bitset

    !>Returns new `operable_bitset` instance having the bits
    !>set to the bitwise logical complement of the bits in the bitset.
    !>
    !>This procedure intented to be overloaded as the `.not.` operator.
    elemental function not_bitset(this) result(new_bitset)
        type(operable_bitset), intent(in) :: this
            !! A bitset at the right side of the NOT operator
        type(operable_bitset) :: new_bitset
            !! new bitset having bits set tothe bitwise logical complement

        call clone(from=this, to=new_bitset)
        call new_bitset%bitset%not()
    end function not_bitset

    !>Returns `.true.` if the all bits in `lhs` and `rhs` have same value,
    !>and returns `.false.` otherwise.
    !>
    !>This procedure intented to be overloaded as the `==` operator
    pure elemental logical function is_equal_bitset_bitset(lhs, rhs)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `==` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `==` operator

        is_equal_bitset_bitset = lhs%bitset == rhs%bitset
    end function is_equal_bitset_bitset

    !>Returns `.true.` if the any bits in `lhs` and `rhs` differ in value,
    !>and returns `.false.` otherwise.
    !>
    !>This procedure intented to be overloaded as the `/=` operator
    pure elemental logical function is_not_equal_bitset_bitset(lhs, rhs)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `/=` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `/=` operator

        is_not_equal_bitset_bitset = lhs%bitset /= rhs%bitset
    end function is_not_equal_bitset_bitset

    !>Returns `.true.` if bitset in `lhs` is greater than that in `rhs`,
    !>and returns `.false.` otherwise.
    !>
    !>This procedure intented to be overloaded as the `>` operator
    pure logical function is_greater_than_bitset_bitset(lhs, rhs)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `>` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `>` operator

        is_greater_than_bitset_bitset = lhs%bitset > rhs%bitset
    end function is_greater_than_bitset_bitset

    !>Returns `.true.` if bitset in `lhs` is greater than or equal to
    !>that in `rhs`, and returns `.false.` otherwise.
    !>
    !>This procedure intented to be overloaded as the `>=` operator
    pure logical function is_greater_than_or_equal_to_bitset_bitset(lhs, rhs)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `>=` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `>=` operator

        is_greater_than_or_equal_to_bitset_bitset = lhs%bitset >= rhs%bitset
    end function is_greater_than_or_equal_to_bitset_bitset

    !>Returns `.true.` if bitset in `lhs` is less than that in `rhs`,
    !>and returns `.false.` otherwise.
    !>
    !>This procedure intented to be overloaded as the `<` operator
    pure logical function is_less_than_bitset_bitset(lhs, rhs)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `<` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `<` operator

        is_less_than_bitset_bitset = lhs%bitset < rhs%bitset
    end function is_less_than_bitset_bitset

    !>Returns `.true.` if bitset in `lhs` is less than or equal to
    !>that in `rhs`, and returns `.false.` otherwise.
    !>
    !>This procedure intented to be overloaded as the `<=` operator
    pure logical function is_less_than_or_equal_to_bitset_bitset(lhs, rhs)
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `<=` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `<=` operator

        is_less_than_or_equal_to_bitset_bitset = lhs%bitset <= rhs%bitset
    end function is_less_than_or_equal_to_bitset_bitset

    !>Performs addition of two bitsets and returns
    !>new `operable_bitset` instance with the value of the result
    !>of the addition.
    !>Addition follows the rules of integer arithmetic,
    !>including overflow.
    !>
    !>This procedure intented to be overloaded as the `+` operator.
    function add_bitset_bitset(lhs, rhs) result(new_bitset)
        use :: bstrith, only:add
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `+` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `+` operator
        type(operable_bitset) :: new_bitset
           !! new bitset having the result of the addition

        new_bitset = operable_bitset(add(lhs%to_string(), rhs%to_string()))
    end function add_bitset_bitset

    !>Performs subtraction of two bitsets and returns
    !>new `operable_bitset` instance with the value of the result
    !>of the subtraction.
    !>Subtraction follows the rules of integer arithmetic,
    !>including underflow.
    !>
    !>This procedure intented to be overloaded as the `-` operator.
    function sub_bitset_bitset(lhs, rhs) result(new_bitset)
        use :: bstrith, only:sub
        type(operable_bitset), intent(in) :: lhs
            !! A bitset at the left side of `-` operator
        type(operable_bitset), intent(in) :: rhs
            !! A bitset at the right side of `-` operator
        type(operable_bitset) :: new_bitset
           !! new bitset having the result of the subtraction

        new_bitset = operable_bitset(sub(lhs%to_string(), rhs%to_string()))
    end function sub_bitset_bitset

    !>Performs bit shift of two bitsets and returns
    !>new `operable_bitset` instance with the value of the result
    !>of the shift.
    !>
    !>`amount > 0` corresponds to a left shift,
    !>`amount < 0` corresponds to a right shift,
    !>`amount ==0` corresponds to no shift.
    !>If absolute value of `amount` is larger than the length of `a`,
    !>the result is undefined.
    !>
    !>This procedure intented to be overloaded as the `.shift.` operator.
    function shift_bitset(this, amount) result(new_bitset)
        use :: bstrith, only:shift
        type(operable_bitset), intent(in) :: this
            !! A bitset at the left side of `.shift.` operator
        integer(int32), intent(in) :: amount
            !! shift amount
        type(operable_bitset) :: new_bitset
           !! new bitset having the result of the bit shift

        new_bitset = operable_bitset(shift(this%to_string(), amount))
    end function shift_bitset

    !>Sorts an input array based on the compoment `bitset` in increasing order,
    !>and sorts the array in decreasing order if `reserve` is `.true.`.
    subroutine sort_bitset(array, reverse)
        use :: stdlib_sorting, only:sort

        type(operable_bitset), intent(inout) :: array(:)
            !! Input array
        logical, intent(in), optional :: reverse
            !! Flag to sort in decreasing order

        type(bitset_large), allocatable :: bitset(:)

        bitset = array(:)%bitset
        call sort(bitset, reverse)
        array(:)%bitset = bitset(:)
    end subroutine sort_bitset
end module orbs_type_operableBitset
