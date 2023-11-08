module orbs_proc_error
    use, intrinsic :: iso_fortran_env
    use :: stdlib_bitsets, only: &
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
        write_failure
    implicit none
    private
    public :: get_error_message
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

    integer(int32), public, parameter :: invalid_format_descriptor = 100
        !! Error flag indicating invalid format descriptor
    integer(int32), public, parameter :: format_descriptor_mismatch = 101
        !! Error flag indicating format descriptor mismatch

contains
    !>Returns a error message corresponding to the error status.
    pure function get_error_message(status) result(message)
        implicit none
        integer(int32), intent(in) :: status
            !! error status
        character(:), allocatable :: message
            !! error message

        select case (status)
        case (success)
            message = "No errors occurred."

        case (alloc_fault)
            message = "A memory allocation failed."

        case (array_size_invalid_error)
            message = "An array size was invalid."

        case (char_string_invalid_error)
            message = "A character string had an invalid character."

        case (char_string_too_large_error)
            message = "A character string was too large."

        case (char_string_too_small_error)
            message = "A character string was too small."

        case (eof_failure)
            message = "An End-Of-File failure occurred on a READ statement."

        case (index_invalid_error)
            message = "An index was invalid."

        case (integer_overflow_error)
            message = "An integer overflow error occurred."

        case (read_failure)
            message = "A failure occurred in a READ statement."

        case (write_failure)
            message = "A failure occurred on a WRITE statement."

        case (invalid_format_descriptor)
            message = "A format descriptor was invalid."

        case (format_descriptor_mismatch)
            message = "A format descriptor was mismatched."

        case default
            message = "Unkown error occurred."

        end select
    end function get_error_message
end module orbs_proc_error
