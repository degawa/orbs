module orbs
    use :: orbs_type_operableBitset
    use :: orbs_proc_literal
    use :: orbs_proc_error, orbs_get_error_message => get_error_message, &
        orbs_success => success, &
        orbs_alloc_fault => alloc_fault, &
        orbs_array_size_invalid_error => array_size_invalid_error, &
        orbs_char_string_invalid_error => char_string_invalid_error, &
        orbs_char_string_too_large_error => char_string_too_large_error, &
        orbs_char_string_too_small_error => char_string_too_small_error, &
        orbs_eof_failure => eof_failure, &
        orbs_index_invalid_error => index_invalid_error, &
        orbs_integer_overflow_error => integer_overflow_error, &
        orbs_read_failure => read_failure, &
        orbs_write_failure => write_failure, &
        orbs_invalid_format_descriptor => invalid_format_descriptor, &
        orbs_format_descriptor_mismatch => format_descriptor_mismatch
    implicit none
    private
    ! type
    public :: operable_bitset

    ! procedures
    public :: orbs_one
    public :: orbs_zero
    public :: orbs_get_error_message
    public :: sort

    ! opereators
    public :: assignment(=)
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

    ! parameter
    public :: orbs_success
    public :: orbs_alloc_fault
    public :: orbs_array_size_invalid_error
    public :: orbs_char_string_invalid_error
    public :: orbs_char_string_too_large_error
    public :: orbs_char_string_too_small_error
    public :: orbs_eof_failure
    public :: orbs_index_invalid_error
    public :: orbs_integer_overflow_error
    public :: orbs_read_failure
    public :: orbs_write_failure
    public :: orbs_invalid_format_descriptor
    public :: orbs_format_descriptor_mismatch
end module orbs
