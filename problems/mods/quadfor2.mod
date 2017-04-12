var w1;
var w2;
var x1;
var x2;

subject to

cons1:  w1 + w2 - 1 = 0;
cons2:  w1*x1 + w2*x2 = 0;
cons3:  w1*x1^2 + w2*x2^2 - 2/3 = 0;
cons4:  w1*x1^3 + w2*x2^3 = 0;

#solve;
#display w1, w2, x1, x2;

# TITLE : Gaussian quadrature formula with 2 knots and 2 weights over [-1,+1].

# ROOT COUNTS :

# total degree : 24
# 2-homogeneous Bezout number : 11
#    with partition : {{w1 w2 }{x1 x2 }}
# mixed volume : 4

# REFERENCE : 

# Jan Verschelde and Karin Gatermann:
# `Symmetric Newton Polytopes for Solving Sparse Polynomial Systems',
# Adv. Appl. Math., 16(1): 95-127, 1995.

# THE GENERATING SOLUTIONS :

# 1 4
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 2
# the solution for t :
#  w1 :  5.00000000000000E-01   0.00000000000000E+00
#  w2 :  5.00000000000000E-01   0.00000000000000E+00
#  x1 :  8.16496580927726E-01   0.00000000000000E+00
#  x2 : -8.16496580927726E-01   0.00000000000000E+00
# == err :  0.000E+00 = rco :  1.249E-01 = res :  0.000E+00 ==
