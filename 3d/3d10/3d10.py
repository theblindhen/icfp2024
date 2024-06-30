#!/usr/bin/env python3

import sys

def has_balanced_brackets(string):
    """
    Check if a string consists of balanced parentheses and brackets. The only
    possible characters in the string are '(', ')', '[', and ']'.
    """
    # A stack to keep track of opened parentheses and brackets
    stack = []
    
    for char in string:
        if char in '([':
            stack.append(char)
        elif char in ')]':
            if not stack:
                return False
            if (char == ')' and stack[-1] == '(') or (char == ']' and stack[-1] == '['):
                stack.pop()
            else:
                return False
    
    return not stack

# A number `A` consisting of digits
# - `1` (representing `(`),
# - `2` (representing `)`),
# - `3` (representing `[`) and
# - `4` (representing `]`), at most `40` digits long.

def has_balanced_numbers(A):
    """
    Check if a number consists of balanced parentheses and brackets. The only
    possible digits in the number are 1, 2, 3, and 4.
    """
    stack = 0

    while True:
        if A == 0:
            # TODO: how to test a large number for 0?
            # Maybe return it fast if it equals 0, else return (1 % stack) one step later.
            return stack == 0
        
        # Get the last digit of the number, so we're moving right to left
        digit = A % 10

        if digit % 2 == 0:
            # Even numbers are close parentheses
            stack = stack * 10 + digit
        else:
            # Odd numbers are open parentheses
            #if stack == 0: # Covered by the next test, it turns out
            #    return False
            top = stack % 10
            if digit + 1 == top:
                stack //= 10
            else:
                return False
        
        A //= 10

# Take the first command line argument into variable A
A = int(sys.argv[1])
#A = 12
print(has_balanced_numbers(A))
