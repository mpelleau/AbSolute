# **************************************************************************
# File: Combustion
# Name: Combustion problem for a temperature of 3000 degrees
# 
# Reference :
# 
# @article{    VHenMicBen97,
#    author    = "Pascal {Van Hentenryck} and Laurent Michel 
#                 and Fr\'ed\'eric Benhamou",
#    title     = "{\tt Newton} - {C}onstraint {P}rogramming over non-linear 
#                                {C}onstraints",
#    journal   = "Scientific Programming",
#    year      = 1997,
#    note      = "Forthcoming"
# }
# 
# ****************************************************************************
# 
# Solution:
# Variables: x1,x2,...,x10
# 
# ***************************************************************************

# Domains
var X1  >= 0, <= 1.0e8;
var X2  >= 0, <= 1.0e8;
var X3  >= 0, <= 1.0e8;
var X4  >= 0, <= 1.0e8;
var X5  >= 0, <= 1.0e8;
var X6  >= 0, <= 1.0e8;
var X7  >= 0, <= 1.0e8;
var X8  >= 0, <= 1.0e8;
var X9  >= 0, <= 1.0e8;
var X10 >= 0, <= 1.0e8;

subject to
cons1 : X2 + 2*X6 + X9 + 2*X10           = 1.0e-5;
cons2 : X3 + X8    = 3.0e-5;
cons3 : X1 + X3 + 2*X5 + 2*X8 + X9 + X10 = 5.0e-5;
cons4 : X4 + 2*X7  = 1.0e-5;
cons5 : 0.5140437e-7 *X5  = X1^2;
cons6 : 0.1006932e-6 *X6  = 2*X2^2;
cons7 : 0.7816278e-15*X7  = X4^2;
cons8 : 0.1496236e-6 *X8  = X1*X3;
cons9 : 0.6194411e-7 *X9  = X1*X2;
cons10 : 0.2089296e-14*X10 = X1*X2^2;

# 
# ,
# 	X1*X3 + 0.1496236e-6*X3       = 0.1496236e-6*0.00003  ,
# 	1.0e10*X4^2 + 0.3908139e-5*X4 = 0.3908139e-10
# 






#solve;
#display X1, X10, X2, X3, X4, X5, X6, X7, X8, X9;
