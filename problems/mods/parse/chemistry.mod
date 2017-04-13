# **************************************************************************
# File: Chemistry
# Name: Chemical equilibrium of hydrocarbon combustion
# 
# Reference :
# 
# @article{VHenMicBen97,
# author = "Pascal {Van Hentenryck} and Laurent Michel 
#           and Fr\'ed\'eric Benhamou",
# title  = "{\tt Newton} - {C}onstraint {P}rogramming over non-linear 
#           {C}onstraints",
# journal= "Scientific Programming",
# year   = 1997
# }
# 
# This polynomial system describes the equilibrium of the products
# of hydrocarbon combustion.
# 
# Keith Meintjes and Alexander P. Morgan:
#  "Chemical equilibrium systems as numerical test problems",
#  ACM Toms, Vol 16, No 2, 143-151, 1990.
# 
# Although the total degree equals 108, there are only 4 real and
# 12 complex solutions and an infinite number of solutions at infinity.
# ***************************************************************************


# Domains
var x1 >= 0, <= 1.0e8;
var x2 >= 0, <= 1.0e8;
var x3 >= 0, <= 1.0e8;
var x4 >= 0, <= 1.0e8;
var x5 >= 0, <= 1.0e8;

# Constants
param R := 10;
param R5 := 0.193;
param R6 := 0.002597/sqrt(40);
param R7 := 0.003448/sqrt(40);
param R8 := 0.00001799/40;
param R9 := 0.0002155/sqrt(40);
param R10 := 0.00003846/40;

subject to
cons1 : 3*x5 = x1*(x2 + 1);

cons2 : x3*(x2*(2*x3+R7) + 2*R5*x3 + R6) =  8*x5;

cons3 : x4*(R9*x2 + 2*x4) = 4*R*x5;

cons4 : x2*(2*x1 + x3*(x3+R7) + R8 + 2*R10*x2 + R9*x4) + x1 = R*x5;


cons5 : x2*(x1 + R10*x2 + x3*(x3+R7) + R8 + R9*x4) +x1 + x3*(R5*x3 + R6) + x4^2 = 1;


# 
# , 51*x5 +  x2*(R8 + R10*x2) - x4^2 - x3^2*(x2 + R5)  = 1 ,
# x2*(x3^2 - R8) - R6*x3 - x1 - 58*x5 + 2 = 0
# 




#solve;
#display x1, x2, x3, x4, x5;
