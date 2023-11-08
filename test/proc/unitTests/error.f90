module test_proc_unitTests_error
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: fassert
    use :: orbs_proc_error
    implicit none
    private
    public :: get_error_message_returns_errmsg

contains
    subroutine get_error_message_returns_errmsg(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(get_error_message(success), &
                          "No errors occurred.", &
                          "error message for success case should be "// &
                          "'No errors occurred.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(alloc_fault), &
                          "A memory allocation failed.", &
                          "error message for acclocation failure case should be "// &
                          "'A memory allocation failed.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(array_size_invalid_error), &
                          "An array size was invalid.", &
                          "error message for invalid array size case should be "// &
                          "'An array size was invalid.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(char_string_invalid_error), &
                          "A character string had an invalid character.", &
                          "error message for invalid character sting case should be "// &
                          "'A character string had an invalid character.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(char_string_too_large_error), &
                          "A character string was too large.", &
                          "error message for too large character string case should be "// &
                          "'A character string was too large.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(char_string_too_small_error), &
                          "A character string was too small.", &
                          "error message for too small character string case should be "// &
                          "'A character string was too small.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(eof_failure), &
                          "An End-Of-File failure occurred on a READ statement.", &
                          "error message for EOF failure case should be "// &
                          "'An End-Of-File failure occurred on a READ statement.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(index_invalid_error), &
                          "An index was invalid.", &
                          "error message for invalid array index case should be "// &
                          "'An index was invalid.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(integer_overflow_error), &
                          "An integer overflow error occurred.", &
                          "error message for integer overflow case should be "// &
                          "'An integer overflow error occurred.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(read_failure), &
                          "A failure occurred in a READ statement.", &
                          "error message for read failure case should be "// &
                          "'A failure occurred in a READ statement.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(write_failure), &
                          "A failure occurred on a WRITE statement.", &
                          "error message for write failure case should be "// &
                          "'A failure occurred on a WRITE statement.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(invalid_format_descriptor), &
                          "A format descriptor was invalid.", &
                          "error message for invalid format descriptor case should be "// &
                          "'A format descriptor was invalid.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(format_descriptor_mismatch), &
                          "A format descriptor was mismatched.", &
                          "error message for format descriptor mismatch case should be "// &
                          "'A format descriptor was mismatched.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(get_error_message(-1), &
                          "Unkown error occurred.", &
                          "error message for unknown error case should be "// &
                          "'Unkown error occurred.'", &
                          stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine get_error_message_returns_errmsg
end module test_proc_unitTests_error
