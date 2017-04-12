# 
# The Circle Theorem of Apollonius, page 284
# 
# @book{                    CoxIdeals,
#    author       = "D. Cox and J. Little and D. {O'Shea}",
#    title        = "{I}deals, {V}arieties and {A}lgorithms",
#    publisher    = "Springer-Verlag",
#    adress       = "NY",
#    year         = 1992 
# }
# 


# Domains
var x1 >= -1.0e8, <= 1.0e8;
var x2 >= -1.0e8, <= 1.0e8;
var x3 >= -1.0e8, <= 1.0e8;
var x4 >= -1.0e8, <= 1.0e8;
var x5 >= -1.0e8, <= 1.0e8;
var x6 >= -1.0e8, <= 1.0e8;
var x7 >= -1.0e8, <= 1.0e8;
var x8 >= -1.0e8, <= 1.0e8;


# Constants
param u1 := 2;
param u2 := 1;

subject to
cons1 : 2*x1 = u1;
cons2 : 2*x2 = u2;
cons3 : 2*x3 = u1;
cons4 : 2*x4 = u2;
cons5 : u1*x5 = u2*x6;
cons6 : u2*x5 + u1*x6 = u1*u2;
cons7 : (x1-x7)^2 + x8^2 - x7^2 = (x8-x2)^2;
cons8 : (x1-x7)^2 + x8^2 = (x3-x7)^2 + (x4-x8)^2;

cons9 : x1^2 + x2^2 = 2*x1*x7 - 2*x2*x8;

cons10 : x1^2 - x3^3 - x4^2 = 2*x7*(x1-x3) - 2*x4*x8;

option presolve 0;
#solve;
#display x1, x2, x3, x4, x5, x6, x7, x8;
