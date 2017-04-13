# **************************************************************************
# File: Economics4
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
# ****************************************************************************

# Domains
var x1 >=-100, <= 100;
var x2 >=-100, <= 100;
var x3 >=-100, <= 100;
var x4 >=-100, <= 100;

subject to
cons1 :  (x1*(1 + x2))*x4 = 1;
cons2 :  x2  = x3*((2/3) - x1);
cons3 :  x3*x4 = 3;
cons4 :  x1 + x2 + x3 = -1;

#solve;
#display x1, x2, x3, x4;
