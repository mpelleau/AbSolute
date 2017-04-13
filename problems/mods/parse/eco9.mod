# **************************************************************************
# File: Economics8
# Name: Economics problem
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
# ***************************************************************************


# Domains
var x1 >= -100, <= 100;
var x2 >= -100, <= 100;
var x3 >= -100, <= 100;
var x4 >= -100, <= 100;
var x5 >= -100, <= 100;
var x6 >= -100, <= 100;
var x7 >= -100, <= 100;
var x8 >= -100, <= 100;

subject to
cons1 : x1 + x2*(x1 + x3) + x4*(x3 + x5) + x6*(x5 + x7) = x8*((1/8) - x7);
cons2 : x2 + x3*(x1 + x5) + x4*(x2 + x6) + x5*x7 = x8*((2/8) - x6);
cons3 : x3*(1 + x6) + x4*(x1 + x7) + x2*x5 = x8*((3/8) - x5);
cons4 : x4 + x1*x5 + x2*x6 + x3*x7 = x8*((4/8) - x4);
cons5 : x5 + x1*x6 + x2*x7 = x8*((5/8) - x3);
cons6 : x6 + x1*x7 = x8*((6/8) - x2);
cons7 : x7 = x8*((7/8) - x1);
cons8 : x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 = -1;


#solve;
#display x1, x2, x3, x4, x5, x6, x7, x8;
