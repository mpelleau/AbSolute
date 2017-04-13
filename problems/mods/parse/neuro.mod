# **************************************************************************
# File: Neuro1
# Name: Neurophysiology
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
# Variables: x1,x2,...,x6
# 
# ***************************************************************************


# Domains
var x1 >= -100, <= 100;
var x2 >= -100, <= 100;
var x3 >= -100, <= 100;
var x4 >= -100, <= 100;
var x5 >= -100, <= 100;
var x6 >= -100, <= 100;

# Constants
param C1 := 5;
param C2 := 4;
param C3 := 3;
param C4 := 2;

subject to
cons1 : x1^2 + x3^2             = 1;
cons2 : x2^2 + x4^2             = 1;
cons3 : x5*x1^3 + x6*x2^3       = C2;
cons4 : x5*x1*x3^2 + x6*x4^2*x2 = C3;
cons5 : x5*x3^3 + x6*x4^3       = C1;
cons6 : x5*x1^2*x3 + x6*x2^2*x4 = C4;


cons7 : x1 >= x2;
cons8 : x1 >= 0;
cons9 : x2 >= 0;

# 
# ,
# 
# (315505/27476064)*x5^2 - (41/25872)*x6^4 + (1705883/13738032)*x6^2 - (309546755/183921408) = 0,
# 
# (13334096465/237560625568)*x6^4 - (5528771705/1385193152)*x6^2 + (40652733125/1324616192) = 0
# 
# 





#solve;
#display x1, x2, x3, x4, x5, x6;
