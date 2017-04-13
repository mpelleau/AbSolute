# Domains
var x1 >= -2, <= 3;
var x2 >= -2, <= 3;

subject to
cons1 : 4*x1^3 - 3*x1 - x2 = 0;
cons2 : x1^2 = x2;

#solve;
#display x1, x2;
