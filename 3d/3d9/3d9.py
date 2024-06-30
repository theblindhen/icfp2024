#!/usr/bin/env python3

import sys

def solve(A):
    # BEWARE: balance starts at 1 in the real implementation. This changes
    # constant 98 to 97 and -1 to 0, and it saves the +1 operation.
    balance = 0

    while True:
        # Has to come before the other return
        if A == 0:
            # If balance is positive, return 0 (unbalanced parentheses)
            # If balance is 0, return 1 (balanced parentheses)
            # The number 99 is large enough because A is max 40 digits
            return 1 - (balance + 98) // 99
        
        delta = (A % 10) * 2 - 3
        balance += delta
        if balance == -1:
            return balance + 1
        
        A //= 10

# Take the first command line argument into variable A
A = int(sys.argv[1])

print(solve(A))
