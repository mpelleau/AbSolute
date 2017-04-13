# Domains
var s1 >= -1, <= 1;
var s2 >= -1, <= 1;
var c1 >= -1, <= 1;
var c2 >= -1, <= 1;

# Constants
param a := 1;
param b := 4;
param l1 := 10;
param l2 := 6;

subject to
cons1 : a = l2*(c1*c2 - s1*s2) + l1*c1;
cons2 : b = l2*(c1*s2 + c2*s1) + l1*s1;
cons3 : c1^2 + s1^2 = 1;
cons4 : c2^2 + s2^2 = 1;

# 
# ,c1 - 6*c2 + 4*s1 = 10,
# c2 - (17/24)*s1 - (1/4)*s2 + (5/3) = 0,
# s1 + (320/153)*s2^2 + (6/17)*s2 - (80/81) = 0,
# s2^2 - (239/14400) = 0
# 


#solve;
#display c1, c2, s1, s2;
