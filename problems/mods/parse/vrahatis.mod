# 
#  KEARFOTT (extended function)
#  Article Vrahatis, Solving Systems ...
# 


# Domains
var x1 >= -1000, <= 1000;
var x2 >= -1000, <= 1000;
var x3 >= -1000, <= 1000;
var x4 >= -1000, <= 1000;
var x5 >= -1000, <= 1000;
var x6 >= -1000, <= 1000;
var x7 >= -1000, <= 1000;
var x8 >= -1000, <= 1000;
var x9 >= -1000, <= 1000;

subject to
cons1 : x1^2 = x2;
cons2 : x2^2 = x3;
cons3 : x3^2 = x4;
cons4 : x4^2 = x5;
cons5 : x5^2 = x6;
cons6 : x6^2 = x7;
cons7 : x7^2 = x8;
cons8 : x8^2 = x9;
cons9 : x9^2 = x1;


# 
# ,x9^511 = 1
# 


#solve;
#display x1, x2, x3, x4, x5, x6, x7, x8, x9;
