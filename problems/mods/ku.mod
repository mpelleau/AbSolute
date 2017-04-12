# 
# 10-dimensional system of Ku
# 
# REFERENCE:
#   M.C. Steenkamp :
#   `Die numeriese oplos van stelsels polinoomvergelykings'.
#   Technical report, Nasionale Navorsingsinstituut vir Wiskundige Wetenskappe,
#   Pretoria, 1982.
# 
# NOTE:
#   This system was known as an easy system for elimination, but difficult 
#   for homotopy continuation.  With multi-homogenization, this is also an 
#   easy system for homotopy continuation methods.
# 
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
var x9 >= -1.0e8, <= 1.0e8;
var x10 >= -1.0e8, <= 1.0e8;

subject to
cons1 : 5*x1*(x2+1) + 3*x2  + 55  = 0;
cons2 : x2*(7*x3+9) + 9*x3  + 19  = 0;
cons3 : x3*(3*x4+6) + 5*x4  - 4   = 0;
cons4 : 6*x4*(x5+1) + 7*x5  + 118 = 0;
cons5 : x5*(x6+3)   + 9*x6  + 27  = 0;
cons6 : x6*(6*x7+7) + x7    + 72  = 0;
cons7 : x7*(9*x8+7) + x8    + 35  = 0;
cons8 : 4*x8*(x9+1) + 6*x9  + 16  = 0;
cons9 : x9*(8*x10+4) + 3*x10 - 51  = 0;
cons10 : x1*(3*x10-6) + x10   + 5   = 0;


#  ORIGINAL SYSTEM:
#  5*x1*x2+ 5*x1+ 3*x2+ 55 = 0,
#  7*x2*x3+ 9*x2+ 9*x3+ 19 = 0,
#  3*x3*x4+ 6*x3+ 5*x4-4 = 0,
#  6*x4*x5+ 6*x4+ 7*x5+ 118 = 0,
# x5*x6+ 3*x5+ 9*x6+ 27 = 0,
#  6*x6*x7+ 7*x6+x7+ 72 = 0,
#  9*x7*x8+ 7*x7+x8+ 35 = 0,
#  4*x8*x9+ 4*x8+ 6*x9+ 16 = 0,
#  8*x9*x10+ 4*x9+ 3*x10-51 = 0,
#  3*x1*x10-6*x1+x10+ 5 = 0
# 

#solve;
#display x1, x10, x2, x3, x4, x5, x6, x7, x8, x9;
