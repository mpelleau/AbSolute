# **************************************************************************
# File: Griewank
# Name: Griewank and Osborne's system
# 
# Reference:
# 
# @article{ morgan-singular,
#    author    = "A. Morgan and A. Sommese and C. Wampler",
#    title     = "{Computing} {Singular} {Solutions} to {Polynomial} {Systems}",
#    journal   = "Advances in Applied Mathematics",
#    year      = 1992,
#    volume    = 13,
#    pages     = "305--327"
# }
# ****************************************************************************
# 
# Solution:  ( 0 , 0 )
# Variables: z1, z2
# 
# ***************************************************************************




# Domains
var z1 >= -1.0e8, <= 1.0e8;
var z2 >= -1.0e8, <= 1.0e8;

subject to
cons1 : 1.8125*z1^3 = 2*z1*z2;
cons2 : z1^2        = z2;


#solve;
#display z1, z2;
