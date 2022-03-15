'''

gen_float_tests.py

This file generates add_vectors.txt and mult_vectors.txt, used as vector inputs to
the floating point adder and multiplier testbenches respectively. The test converage
consists of the following edge cases for multiplication (where nmin is the smallest
nonzero floating point value and nmax is the largest non-infinity value):

0*0=0
0*nmin=0
nmax*0=0
2^{-14}*2^{-14}=0
2^{-14}*2^{-1}=0
(n <= 2^{-8})*(n <= 2^{-8})=0
inf*nmin = inf
nmax*inf = inf
nmax*nmax = inf
nmax*1.5 = inf
(n >= 2^8)*(n >= 2^8) = inf

and for addition:

0+0 = 0
0-0 = 0
inf+n=inf with n in {0,-0,inf,-inf,random value,random value}
-inf+n=-inf with n in {0,-0,inf,-inf,random value,random value}
n+inf=inf with n in {0,-0,random value,random value}
n-inf=-inf with n in {0,-0,random value,random value}
n-n=0 with n in {2^{-14},-2^{-14},1,-1,nmax,-nmax}
2^{-14}-(2^{-14}+2^{-15})=0
-2^{-14}+(2^{-14}+2^{-15})=0
2^{-14}-(2^{-14}+2^{-24})=0
-2^{-14}+(2^{-14}+2^{-24})=0
nmax+nmax=inf
-nmax-nmax=-inf
nmax+2^5=inf
-nmax-2^5=-inf
2^{15}+2^{15}=inf
-2^{15}-2^{15}=-inf

Each set of test vectors includes 100 randomly generated vectors (where exponent,
mantissa, and sign are independently picked from uniform distributions across their
range) in addition to the edge cases.

Revision History:
    30 Apr 21  Ray Wendt        Initial Revision
     2 May 21  Ray Wendt        Added edge case multiplication tests
     3 May 21  Ray Wendt        Added random multiplication tests and export to file
     4 Jun 21  Ray Wendt        Added addition tests
    16 Jun 21  Ray Wendt        Added header comment

'''

import numpy as np

# gets the normalized exponent of a half-precision float
def float_exponent(f):
    return (((f.view(np.int16) & 0b0111110000000000) >> 10) - 15)

# gets the mantissa of a half-precision float, as a half-precision float
def float_mantissa(f):
    if float_exponent(f) > 0:
        # not subnormal
        return np.float16((f.view(np.int16) & 0b0000001111111111)*(2**-10) + 1.0)
    else:
        # subnormal, no 1.
        return np.float16((f.view(np.int16) & 0b0000001111111111)*(2**-10))

# constructs a half-precision float from sign (s), exponent (e), and mantissa (m)
# m should be float16, e integer, s integer +-1
def construct_float(s, e, m):
    return np.float16(s)*np.float16(2**e)*m

# generates a random half-precision float with exponent in [e_low,e_high) where
# the exponents have a uniform distribution in that range and mantissas have a 
# uniform distribution across their entire range [1.0,2.0)
def rand_float(e_low, e_high):
    # random exponent in range
    e = np.random.randint(e_low, e_high)
    # random mantissa in [1.0,2.0)
    m = np.float16(1.0 + np.random.rand())
    # random sign either -1 or 1
    s = (-1)**np.random.randint(0,2)

    return construct_float(s,e,m)

def export_tests(tests, filename):
    with open(filename, "w") as f:
        # output each test as binary values
        for tst in tests:
            f.write(f'{tst["A"].view(np.uint16):016b} {tst["B"].view(np.uint16):016b} {tst["R"].view(np.uint16):016b}\n')


# number of random tests to generate
N_RAND_TESTS = 100

def gen_mult_tests():
    # list of dictionaries with operands given by keys "A", "B" and result "R"
    tests = []

    # edge cases:
    # 0*0 = 0
    tests.append({"A":np.float16(0.0),      "B":np.float16(0.0),        "R":np.float16(0.0)})
    # 0*(smallest n) = 0
    tests.append({"A":np.float16(0.0),      "B":np.float16(2.0**-14),   "R":np.float16(0.0)})
    # (largest n)*0 = 0
    tests.append({"A":np.float16(65500.0),  "B":np.float16(0.0),        "R":np.float16(0.0)})
    # (2^-14)*(2^-14) = 0
    tests.append({"A":np.float16(2.0**-14), "B":np.float16(2.0**-14),   "R":np.float16(0.0)})
    # (2^-14)*(2^-1) = 0
    tests.append({"A":np.float16(2.0**-14), "B":np.float16(2.0**-1),    "R":np.float16(0.0)})
    # (n <= 2^-8)*(n <= 2^-8) = 0
    op_a = rand_float(-14,-7)
    op_b = rand_float(-14,-7)
    tests.append({"A":op_a,                 "B":op_b,                   "R":np.float16(0.0)*np.sign(op_a)*np.sign(op_b)})
    # inf*(smallest n) = inf
    tests.append({"A":np.float16('inf'),    "B":np.float16(2.0**-14),   "R":np.float16('inf')})
    # (largest n)*inf = inf
    tests.append({"A":np.float16(65500.0),  "B":np.float16('inf'),      "R":np.float16('inf')})
    # (largest n)*(largest n) = inf
    tests.append({"A":np.float16(65500.0),  "B":np.float16(65500.0),    "R":np.float16('inf')})
    # (largest n)*1.5 = inf
    tests.append({"A":np.float16(65500.0),  "B":np.float16(1.5),        "R":np.float16('inf')})
    # (n >= 2^8)*(n >= 2^8) = inf
    op_a = rand_float(8,16)
    op_b = rand_float(8,16)
    tests.append({"A":op_a,                 "B":op_b,                   "R":np.float16('inf')*np.sign(op_a)*np.sign(op_b)})

    # generate random tests
    for i in range(N_RAND_TESTS):
        # generate random operands and product
        op_a = rand_float(-14,15)
        op_b = rand_float(-14,15)
        prod = op_a*op_b

        # if product is denormal clamp it to appropriately signed 0
        if float_exponent(prod) == -15:
            prod = np.float16(0.0)*np.sign(prod)

        # add to tests
        tests.append({"A":op_a, "B":op_b, "R":prod})

    # write vectors to file
    export_tests(tests, "mult_vectors.txt")

def gen_add_tests():
    # list of dictionaries with operands given by keys "A", "B" and result "R"
    tests = []

    # edge cases:
    # 0+0 = 0
    tests.append({"A":np.float16(0.0),      "B":np.float16(0.0),        "R":np.float16(0.0)})
    # 0-0 = 0
    tests.append({"A":np.float16(0.0),      "B":np.float16(-0.0),       "R":np.float16(0.0)})
    # inf+any value=inf
    tests.append({"A":np.float16('inf'),    "B":np.float16(0.0),        "R":np.float16('inf')})
    tests.append({"A":np.float16('inf'),    "B":np.float16(-0.0),       "R":np.float16('inf')})
    tests.append({"A":np.float16('inf'),    "B":np.float16('inf'),      "R":np.float16('inf')})
    tests.append({"A":np.float16('inf'),    "B":np.float16('-inf'),     "R":np.float16('inf')})
    tests.append({"A":np.float16('inf'),    "B":rand_float(-14,15),     "R":np.float16('inf')})
    tests.append({"A":np.float16('inf'),    "B":rand_float(-14,15),     "R":np.float16('inf')})
    # -inf+any value=inf
    tests.append({"A":np.float16('-inf'),   "B":np.float16(0.0),        "R":np.float16('-inf')})
    tests.append({"A":np.float16('-inf'),   "B":np.float16(-0.0),       "R":np.float16('-inf')})
    tests.append({"A":np.float16('-inf'),   "B":np.float16('inf'),      "R":np.float16('-inf')})
    tests.append({"A":np.float16('-inf'),   "B":np.float16('-inf'),     "R":np.float16('-inf')})
    tests.append({"A":np.float16('-inf'),   "B":rand_float(-14,15),     "R":np.float16('-inf')})
    tests.append({"A":np.float16('-inf'),   "B":rand_float(-14,15),     "R":np.float16('-inf')})
    # (any non-inf value)+inf=inf
    tests.append({"A":np.float16(0.0),      "B":np.float16('inf'),      "R":np.float16('inf')})
    tests.append({"A":np.float16(-0.0),     "B":np.float16('inf'),      "R":np.float16('inf')})
    tests.append({"A":rand_float(-14,15),   "B":np.float16('inf'),      "R":np.float16('inf')})
    tests.append({"A":rand_float(-14,15),   "B":np.float16('inf'),      "R":np.float16('inf')})
    # (any non-inf value)-inf=-inf
    tests.append({"A":np.float16(0.0),      "B":np.float16('-inf'),     "R":np.float16('-inf')})
    tests.append({"A":np.float16(-0.0),     "B":np.float16('-inf'),     "R":np.float16('-inf')})
    tests.append({"A":rand_float(-14,15),   "B":np.float16('-inf'),     "R":np.float16('-inf')})
    tests.append({"A":rand_float(-14,15),   "B":np.float16('-inf'),     "R":np.float16('-inf')})
    # n-n = 0
    tests.append({"A":np.float16(2.0**-14), "B":np.float16(-2.0**-14),  "R":np.float16(0.0)})
    tests.append({"A":np.float16(-2.0**-14),"B":np.float16(2.0**-14),   "R":np.float16(0.0)})
    tests.append({"A":np.float16(1.0),      "B":np.float16(-1.0),       "R":np.float16(0.0)})
    tests.append({"A":np.float16(-1.0),     "B":np.float16(1.0),        "R":np.float16(0.0)})
    tests.append({"A":np.float16(65500.0),  "B":np.float16(-65500.0),   "R":np.float16(0.0)})
    tests.append({"A":np.float16(-65500.0), "B":np.float16(65500.0),    "R":np.float16(0.0)})
    # difference < 2^-14 = 0
    tests.append({"A":np.float16(2.0**-14), "B":np.float16(-(2.0**-14+2.0**-15)),    
                                                                        "R":np.float16(-0.0)})
    tests.append({"A":np.float16(-2.0**-14),"B":np.float16(2.0**-14+2.0**-15),    
                                                                        "R":np.float16(0.0)})
    tests.append({"A":np.float16(2.0**-14), "B":np.float16(-(2.0**-14+2.0**-24)),    
                                                                        "R":np.float16(-0.0)})
    tests.append({"A":np.float16(-2.0**-14),"B":np.float16(2.0**-14+2.0**-24),    
                                                                        "R":np.float16(0.0)})
    # overflow to infinity
    tests.append({"A":np.float16(65500.0),  "B":np.float16(65500.0),    "R":np.float16('inf')})
    tests.append({"A":np.float16(-65500.0), "B":np.float16(-65500.0),   "R":np.float16('-inf')})
    tests.append({"A":np.float16(65500.0),  "B":np.float16(2.0**5),     "R":np.float16('inf')})
    tests.append({"A":np.float16(-65500.0), "B":np.float16(-2.0**5),    "R":np.float16('-inf')})
    tests.append({"A":np.float16(2.0**15),  "B":np.float16(2.0**15),    "R":np.float16('inf')})
    tests.append({"A":np.float16(-2.0**15), "B":np.float16(-2.0**15),   "R":np.float16('-inf')})

    # generate random tests
    for i in range(N_RAND_TESTS):
        # generate random operands and sum
        op_a = rand_float(-14,15)
        op_b = rand_float(-14,15)
        op_sum = op_a + op_b

        # if sum is denormal clamp it to appropriately signed 0
        if float_exponent(op_sum) == -15:
            op_sum = np.float16(0.0)*np.sign(op_sum)

        # add to tests
        tests.append({"A":op_a, "B":op_b, "R":op_sum})

    # write vectors to file
    export_tests(tests, "add_vectors.txt")

if __name__ == "__main__":
    np.random.seed(2021)

    gen_mult_tests()
    gen_add_tests()