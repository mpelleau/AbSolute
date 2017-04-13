# 
# Title: a stationary chemical kinetics problem
# 
# Reference:
#  SC 88-07 Herbert Melenk, H. Michael Moeller, Winfried Neun:
#  Symbolic Solution of Large Stationary Chemical Kinetics Problems.
#  Appeared in: IMPACT Comp. Sci. Eng. 1, p.138-167 (1989) 
# 
# See Jan Verschelde
# 



# Domains
var x3 >= -1.0e8, <= 1.0e8;
var x4 >= -1.0e8, <= 1.0e8;
var y2 >= -1.0e8, <= 1.0e8;
var y3 >= -1.0e8, <= 1.0e8;
var y4 >= -1.0e8, <= 1.0e8;
var y5 >= -1.0e8, <= 1.0e8;
var z2 >= -1.0e8, <= 1.0e8;
var z3 >= -1.0e8, <= 1.0e8;
var z4 >= -1.0e8, <= 1.0e8;
var z5 >= -1.0e8, <= 1.0e8;


subject to
cons1 : 9*y2^2 + z2 = 5.656854249492381*y2;


cons2 : x3^2 + y3^2 + z3^2 = 1;
cons3 : x4^2 + y4^2 + z4^2 = 1;
cons4 : y5^2 + z5^2 = 0.888888888888889;
cons5 : x3*(1 - 2.828427124746190*y2) + y2*y3 + z2*z3 = 1/3;
cons6 : x3*x4 + y3*y4 + z3*z4 = 1/3;
cons7 : 1/3*x4 + y4*y5 + z4*z5 =  1/3;
cons8 : 8/3 + x3 + x4 = 2.828427124746190*y2;
cons9 : y2 + y3 + y4 + y5 + 0.8888888888888889 = 0;
cons10 : z2 + z3 + z4 + z5 = 0;


#  ORIGINAL SYSTEM:
# 9*y2^2 + z2 = 5.656854249492381*y2,
# x3^2 + y3^2 + z3^2 = 1,
# x4^2 + y4^2 + z4^2 = 1,
# y5^2 + z5^2 = 0.888888888888889,
# x3 - 2.828427124746190*y2*x3 + y2*y3 + z2*z3 - 1/3 = 0,
# x3*x4 + y3*y4 + z3*z4 = 1/3,
# 1/3*x4 + y4*y5 + z4*z5 =  1/3,
# 8/3 + x3 + x4 = 2.828427124746190*y2,
# y2 + y3 + y4 + y5 + 0.8888888888888889 = 0,
# z2 + z3 + z4 + z5 = 0
# 



#solve;
#display x3, x4, y2, y3, y4, y5, z2, z3, z4, z5;
