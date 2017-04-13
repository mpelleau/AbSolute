# Kearfott
# 
#  11. Kinematics problem
# 




# Domains
var x1 >= -1, <= 1;
var x2 >= -1, <= 1;
var x3 >= -1, <= 1;
var x4 >= -1, <= 1;
var x5 >= -1, <= 1;
var x6 >= -1, <= 1;
var x7 >= -1, <= 1;
var x8 >= -1, <= 1;

subject to
cons1 : x1^2 + x2^2 = 1;
cons2 : x3^2 + x4^2 = 1;
cons3 : x5^2 + x6^2 = 1;
cons4 : x7^2 + x8^2 = 1;
   
cons5 : 0.004731*x1*x3 - 0.3578*x2*x3-0.1238*x1-0.001637*x2-0.9338*x4+x7-0.3571 = 0;

cons6 : 0.2238*x1*x3 + 0.7623*x2*x3 + 0.2638*x1 - 0.07745*x2 - 0.6734*x4 - 0.6022 = 0;

cons7 : x6*x8 + 0.3578*x1 + 0.004731*x2 = 0;

cons8 : -0.7623*x1 + 0.2238*x2 + 0.3461 = 0;


#solve;
#display x1, x2, x3, x4, x5, x6, x7, x8;
