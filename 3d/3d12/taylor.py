import math
import random

T = 1_000_000_000

def sin_taylor(A):
  # Taylor series
  # x - x^3/6 + x^5/120 - x^7/5040 + x^9/362880 - x^11/39916800 + x^13/6227020800 + O(x^15)

  ### Straightforward implementation using rationals
  # x = A / T
  # taylor = x - x**3/6 + x**5/120 - x**7/5040 + x**9/362880 - x**11/39916800 + x**13/6227020800
  # return math.floor(taylor * T)

  ### Semi-integer implementation
  # def fac_from(a, b):
  #   res = 1
  #   for i in range(a, b+1):
  #     res *= i
  #   return res
  # scaled = sum([ T**(13-(2*k+1)) * A**(2*k+1) / fac_from(1, 1+2*k) * (-1)**k for k in range(7)])
  # return math.floor(scaled / (T**12))

  ### Integer implementation
  def fac_from(a, b):
    res = 1
    for i in range(a, b+1):
      res *= i
    return res
  # scaled = sum([ (-1)**k * T**(13-(2*k+1)) * fac_from(1+2*k+1, 13) * A**(2*k+1) for k in range(7)])
  # return scaled // (fac_from(1, 13) * T**12)
  
  ### Horner integer implementation
  K = 1
  sign = 1
  poly = A * A
  for r in range(1, 7):  # r = 6-k according to above
    K *= T**2 * (15 - 2*r) * (14 - 2*r)
    sign *= -1
    poly += sign * K
    if r != 6:
      poly *= A * A
  return poly * A // K




def sin(A):
    # Use the stdlibrary 
    return math.floor(math.sin(A / T) * T)
    
for i in range(1, 1_000):
    # draw a random number between
    # -1570796327 and 1570796327
    A = random.randint(-1570796327, 1570796327)
    taylor = sin_taylor(A)
    ref = sin(A)
    assert abs(taylor - ref) <= 1 , f"Error on A={A}:    {taylor} != {ref}"