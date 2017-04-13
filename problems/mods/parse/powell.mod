# Domains
var x1 >= -2, <= 2;
var x2 >= -2, <= 2;
var x3 >= -2, <= 2;
var x4 >= -2, <= 2;


subject to
cons1 : 10*x2 = -x1;
cons2 : x3 = x4;
cons3 : x2^2 + 4*x3^2 = 4*x2*x3;
cons4 : x1^2 + x4^2 = 2*x1*x4;

#solve;
#display x1, x2, x3, x4;
