# 
# TITLE : Gaussian quadrature formula with 2 knots and 2 weights over [-1,+1].
# 
# REFERENCE: 
# Jan Verschelde and Karin Gatermann:
# `Symmetric Newton Polytopes for Solving Sparse Polynomial Systems',
# Adv. Appl. Math., 16(1): 95-127, 1995.
# 



# Domains
var w1 >= -1.0e8, <= 1.0e8;
var w2 >= -1.0e8, <= 1.0e8;
var x1 >= -1.0e8, <= 1.0e8;
var x2 >= -1.0e8, <= 1.0e8;



subject to
#  The two following constraints are obtained by GB computations 
cons1 : x2^2  = 2/3;
cons2 : x1+x2 = 0;

#  Original system 
cons3 : w1      + w2      = 1;
cons4 : w1*x1   + w2*x2   = 0;
cons5 : w1*x1^2 + w2*x2^2 = 2/3;
cons6 : w1*x1^3 + w2*x2^3 = 0;

#solve;
#display w1, w2, x1, x2;
