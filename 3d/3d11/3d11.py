#!/usr/bin/env python3

import sys

## Input
#  A number `A` consisting of digits
#  - `1` (representing `U`),
#  - `2` (representing `L`),
#  - `3` (representing `D`) and
#  - `4` (representing `R`), at most `100` digits long.
#
## Output
#  The number of unique positions that are visited by the Lambdaman path represented by `A`.
#
## Example
#  * `A = 33321411`
#    `Answer = 6`

# Multiply/divide by this factor to go up or down
# The map may be 200 x 200, so we need a number that's easy to compute but bigger than 2**200.
row_factor = 2 ** 256

def count_positions(A):
    # We start in the middle
    pos = 1 * row_factor ** 256 * row_factor
    bitmap = pos
    count = 1

    while True:
        digit = A % 10
        if digit == 0:
            return count

        A //= 10

        if digit == 1: # up
            pos //= row_factor
        
        if digit == 2: # left
            pos //= 2

        if digit == 3: # down
            pos *= row_factor

        if digit == 4: # right
            pos *= 2
        
        if (bitmap // pos) % 2 == 0:
            count += 1
            bitmap += pos


# Take the first command line argument into variable A
A = int(sys.argv[1])
#A = 33321411
print(count_positions(A))

