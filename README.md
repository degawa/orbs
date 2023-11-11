# orbs
A wrapper that allows arithmetic operations for stdlib_bitsets.

## Motivation
The motivation behind creating the `orbs` is to enhance the usability and to extend the applicability of the [stdlib](https://github.com/fortran-lang/stdlib) bitset. While the stdlib bitset has enough features to represent and operate bitsets, it does not support shift and arithmetic operations. This wrapper, `orbs`, aims to expand the range of information conveyed by bitsets by providing several operators, including logical, shift, and arithmetic operators on the bitset.

## Overview
The `orbs` provides  a user-defined derived type for handling bitsets, procedures, and operators.

### User-Defined Derived Type
- type
    - `operable_bitset`
- procedures
    - constructors
    - `sort`
    - Formatted write
    - Formatted read (only for NAG Fortran)
    - error status codes and `get_error_message` for getting corresponding error message

### Operators for `operable_bitset` type
- logical operators
    - `.and.`
    - `.andnot.`
    - `.or.`
    - `.xor.`
    - `.not.`
- comparison operators
    - `==`
    - `/=`
    - `<`
    - `<=`
    - `>`
    - `>=`
- bit operators
    - `.shift.`
- arithmetic operators
    - `+`
    - `-`
- catenation operator
    - `//`

### Examples

```Fortran
    use :: orbs
    implicit none

    type(operable_bitset) :: bitset, one

    bitset = operable_bitset("11111111")
    write (*, *) bitset ! 11111111
    write (*, '(DT"bitset"(10))') bitset !   11111111

    one = orbs_one(bitset)
    print *, bitset + one ! 11111111 + 00000001 = 00000000
    print '(DT"bitset"(8))', bitset.shift.2 ! 11111100

    bitset = "00001111" ! Assign a string as a bitset
    print *, bitset ! 00001111
```

```Fortran
    use :: orbs
    implicit none

    type(operable_bitset) :: key, one, mask_x, mask_y

    key = operable_bitset("0011") ! Morton code encoding 2D mesh with 2 levels

    one = orbs_one(mold=key)
    mask_x = operable_bitset("1010")
    mask_y = operable_bitset("0101")

    ! find off-branch neighbours at the same refinement level
    print *, (((key .or. mask_y) + one) .and. mask_x) .or. (key .and. mask_y)
    ! 1001
```

## Getting started
### Requirements
Since `orbs` uses `bitset_large` in the stdlib, an environment that can build at least the stdlib is required. The compilers and versions listed below have been used to develop `orbs`.

- Modern Fortran compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran-stdlib](https://github.com/fortran-lang/stdlib)
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.9.0 alpha

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/orbs.git
cd orbs
```

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

### Reference from your project
Add the following `use` statement to modules or procedures that use `orbs`.

```Fortran
use :: orbs
```

### Reference as a fpm project's dependency
To use `orbs` in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
orbs = {git = "https://github.com/degawa/orbs.git"}
```
